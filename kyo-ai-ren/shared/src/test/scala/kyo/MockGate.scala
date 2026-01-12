package kyo

import kyo.*
import kyo.ai.json.*
import kyo.ren.network.*

class MockGate private (
    responses: List[MockGate.Response],
    historyMaxEntries: Int,
    history: History
) extends Gate:

    def apply[Out: Json](inputs: List[Input])(using Frame) =
        val found =
            responses
                .find(_.inputs.forall(_ == inputs))
                .getOrElse {
                    throw new IllegalStateException(s"Can't find a response for ${inputs.mkString(", ")}")
                }
        val output = found.output.asInstanceOf[Out]
        history(inputs, output).map(_ => output)
    end apply

    def state(using Frame): Gate.State < Sync =
        history.state.map { historyState =>
            Gate.State(
                prompt = "mock prompt",
                reminder = "mock reminder",
                temperature = Temperature.balanced,
                history = historyState
            )
        }

end MockGate

object MockGate:

    case class Response(
        inputs: Option[List[Input]] = None,
        output: Any
    )

    def init(responses: Response*)(using Frame): MockGate < Sync =
        init(5, responses*)

    def init(historyMaxEntries: Int, responses: Response*)(using Frame): MockGate < Sync =
        History.init(History.State(List.empty, historyMaxEntries)).map { history =>
            new MockGate(List.from(responses), historyMaxEntries, history)
        }

    def output(output: String = "", activations: Neuron.Activation*): Neuron.Output =
        if output.isEmpty then
            Neuron.Output.Subconscious(None, Some(List.from(activations)))
        else
            Neuron.Output.Conscious(output, Some(List.from(activations)))

end MockGate
