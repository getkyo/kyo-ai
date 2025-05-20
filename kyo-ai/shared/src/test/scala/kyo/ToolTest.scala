package kyo

import kyo.*
import kyo.ai.*
import kyo.ai.Context.*
import kyo.ai.json.Json
import kyo.ai.json.description

class ToolTest extends Test:

    val testConfig =
        Config.OpenAI.gpt_4o
            .temperature(0.1)
            .seed(Present(42))

    case class CalculateInput(expression: String)
    case class CalculateOutput(result: Double)

    case class WeatherRequest(city: String)
    case class WeatherResponse(temperature: Double, conditions: String)

    case class TranslateInput(text: String, targetLanguage: String)
    case class TranslateOutput(translatedText: String)

    "basic tool usage" in run {
        val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator") { input =>
            val result = input.expression match
                case "1+2"  => 3.0
                case "2*3"  => 6.0
                case "3*4"  => 12.0
                case "10/2" => 5.0
                case _      => 0.0
            CalculateOutput(result)
        }

        AI.run(testConfig) {
            calculatorTool.enable {
                for
                    _ <- AI.systemMessage(
                        "You are a helpful assistant with access to a calculator tool. Use the calculator tool when asked to solve math problems."
                    )
                    _        <- AI.userMessage("What is 2*3? Please show your work and mention which tool you used.")
                    response <- AI.gen[String]
                yield assert(response.contains("6"))
            }
        }
    }

    "multiple tools" in run {
        val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator") { input =>
            val result = input.expression match
                case "1+2"  => 3.0
                case "2*3"  => 6.0
                case "3*4"  => 12.0
                case "10/2" => 5.0
                case _      => 0.0
            CalculateOutput(result)
        }

        val weatherTool = Tool.init[WeatherRequest]("weather", "Get weather information for a city") { request =>
            val result = request.city.toLowerCase match
                case "new york" => WeatherResponse(72.5, "Sunny")
                case "london"   => WeatherResponse(65.0, "Cloudy")
                case "tokyo"    => WeatherResponse(78.0, "Clear")
                case "sydney"   => WeatherResponse(70.0, "Partly cloudy")
                case _          => WeatherResponse(68.0, "Unknown")
            result
        }

        AI.run(testConfig) {
            Tool.enable(calculatorTool, weatherTool) {
                for
                    _ <- AI.systemMessage(
                        "You are a helpful assistant with access to calculator and weather tools. Use the appropriate tool based on the user's question."
                    )
                    _                  <- AI.userMessage("What's the weather in Tokyo?")
                    weatherResponse    <- AI.gen[String]
                    _                  <- AI.userMessage("What is 3*4?")
                    calculatorResponse <- AI.gen[String]
                yield
                    assert(weatherResponse.contains("78") || weatherResponse.contains("Tokyo"))
                    assert(calculatorResponse.contains("12"))
            }
        }
    }

    "tool with custom prompt" in run {
        val customPrompt = Prompt.init(
            p"""
            When using the calculator tool:
            1. Always format expressions with clear syntax
            2. Verify the input is valid before calling the tool
            3. Explain the calculation to the user after showing the result
            """,
            "Remember to format expressions clearly and explain results."
        )

        val calculatorTool = Tool.init[CalculateInput](
            "calculator",
            "A simple calculator that evaluates mathematical expressions",
            customPrompt
        ) { input =>
            val result = input.expression match
                case "1+2"  => 3.0
                case "2*3"  => 6.0
                case "3*4"  => 12.0
                case "10/2" => 5.0
                case _      => 0.0
            CalculateOutput(result)
        }

        AI.run(testConfig) {
            calculatorTool.enable {
                for
                    _        <- AI.systemMessage("You are a helpful assistant with access to a calculator tool.")
                    _        <- AI.userMessage("What is 3*4? Please explain the calculation process.")
                    response <- AI.gen[String]
                yield assert(response.contains("12"))
            }
        }
    }

    "tool aggregation" in run {
        val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator") { input =>
            val result = input.expression match
                case "1+2"  => 3.0
                case "2*3"  => 6.0
                case "3*4"  => 12.0
                case "10/2" => 5.0
                case _      => 0.0
            CalculateOutput(result)
        }

        val weatherTool = Tool.init[WeatherRequest]("weather", "Get weather information for a city") { request =>
            val result = request.city.toLowerCase match
                case "new york" => WeatherResponse(72.5, "Sunny")
                case "london"   => WeatherResponse(65.0, "Cloudy")
                case "tokyo"    => WeatherResponse(78.0, "Clear")
                case "sydney"   => WeatherResponse(70.0, "Partly cloudy")
                case _          => WeatherResponse(68.0, "Unknown")
            result
        }

        val aggregatedTools = Tool.aggregate(calculatorTool, weatherTool)

        AI.run(testConfig) {
            aggregatedTools.enable {
                for
                    _                  <- AI.systemMessage("You are a helpful assistant with access to calculator and weather tools.")
                    _                  <- AI.userMessage("What's the weather in London?")
                    weatherResponse    <- AI.gen[String]
                    _                  <- AI.userMessage("What is 10/2?")
                    calculatorResponse <- AI.gen[String]
                yield
                    assert(weatherResponse.contains("65") || weatherResponse.contains("Cloudy"))
                    assert(calculatorResponse.contains("5"))
            }
        }
    }

    "tool with error handling" in run {
        val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator with error handling") { input =>
            if input.expression.contains("/0") then
                AI.fail("Division by zero is not allowed")
            else
                val result = input.expression match
                    case "1+2"  => 3.0
                    case "2*3"  => 6.0
                    case "10/2" => 5.0
                    case _      => 0.0
                CalculateOutput(result)
        }

        AI.run(testConfig) {
            calculatorTool.enable {
                for
                    _ <- AI.systemMessage(
                        "You are a helpful assistant with access to a calculator tool that does not support division by zero."
                    )
                    _             <- AI.userMessage("What is 10/2?")
                    validResponse <- AI.gen[String]
                    _             <- AI.userMessage("What is 5/0?")
                    errorResponse <- Abort.run(AI.gen[String])
                yield
                    assert(validResponse.contains("5"))
                    assert(errorResponse.isError)
            }
        }
    }

    "translation tool with custom types" in run {
        val translateTool = Tool.init[TranslateInput]("translate", "Translates text to another language") { input =>
            val translation = (input.text.toLowerCase, input.targetLanguage.toLowerCase) match
                case ("hello", "spanish")   => "Hola"
                case ("hello", "french")    => "Bonjour"
                case ("hello", "german")    => "Hallo"
                case ("goodbye", "spanish") => "Adiós"
                case ("goodbye", "french")  => "Au revoir"
                case ("goodbye", "german")  => "Auf Wiedersehen"
                case _                      => s"[Translation of '${input.text}' to ${input.targetLanguage}]"

            TranslateOutput(translation)
        }

        AI.run(testConfig) {
            translateTool.enable {
                for
                    _         <- AI.systemMessage("You are a helpful assistant with access to a translation tool.")
                    _         <- AI.userMessage("How do you say 'hello' in Spanish?")
                    response1 <- AI.gen[String]
                    _         <- AI.userMessage("And how about 'goodbye' in French?")
                    response2 <- AI.gen[String]
                yield
                    assert(response1.contains("Hola"))
                    assert(response2.contains("Au revoir"))
            }
        }
    }

    "thought-driven tool usage" in run {
        case class MathProblem(expression: String)
        case class StepByStepAnalysis(
            `Let me break down this problem step by step`: String,
            `The correct approach to this calculation is`: String
        )

        val calculatorTool = Tool.init[CalculateInput]("calculator", "A simple calculator") { input =>
            val result = input.expression match
                case "1+2"     => 3.0
                case "2*3"     => 6.0
                case "3*4"     => 12.0
                case "(2+3)*4" => 20.0
                case _         => 0.0
            CalculateOutput(result)
        }

        val mathThought = Thought.opening[StepByStepAnalysis]

        AI.run(testConfig) {
            calculatorTool.enable {
                mathThought.enable {
                    for
                        _        <- AI.systemMessage("You are a math tutor that helps students solve math problems step by step.")
                        _        <- AI.userMessage("Calculate (2+3)*4 and provide a detailed explanation of how you solved it.")
                        response <- AI.gen[String]
                    yield assert(response.contains("20"))
                }
            }
        }
    }

end ToolTest
