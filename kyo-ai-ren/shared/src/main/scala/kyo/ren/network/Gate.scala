package kyo.ren.network

import kyo.*
import kyo.ai.json.*
import kyo.ai.json.doc

trait Gate:

    def apply[Out: Json](inputs: List[Input])(using Frame): Out < AI

    def state(using Frame): Gate.State < Sync

end Gate

object Gate:

    val inputsAspect =
        given Frame = Frame.internal
        Aspect.init[Const[List[Input]], Id, AI]

    case class State(
        @doc("System prompt that defines the gate's behavior")
        prompt: String,
        @doc("Brief reminder of important aspects in the prompt")
        reminder: String,
        temperature: Temperature,
        history: History.State
    ) derives CanEqual, Json:
        require(prompt.nonEmpty, "Gate prompt cannot be empty")
    end State

    def init(initState: State)(using Frame): Gate < Sync =
        val prompt = Prompt.init(initState.prompt, initState.reminder)
        History.init(initState.history).map { history =>
            new Gate:
                def apply[Out: Json](inputs: List[Input])(using Frame) =
                    for
                        output <-
                            AI.fresh(
                                AI.withConfig(_.temperature(initState.temperature.normalized)) {
                                    history.state.map(history =>
                                        inputsAspect(inputs) { inputs =>
                                            prompt.enable(
                                                AI.gen[Out](
                                                    History.toMarkdown(history.entries) + "\n\n" +
                                                        Input.toMarkdown(inputs)
                                                )
                                            )
                                        }
                                    )
                                }
                            )
                        _ <- history(inputs, output)
                    yield output
                    end for
                end apply

                def state(using Frame): Gate.State < Sync =
                    history.state.map(h => initState.copy(history = h))
        }
    end init

    private case class GenInput(
        history: String,
        inputs: String
    ) derives CanEqual, Json
end Gate
