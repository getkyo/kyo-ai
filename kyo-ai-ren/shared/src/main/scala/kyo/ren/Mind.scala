package kyo.ren

import kyo.*
import kyo.ai.json.*
import kyo.ren.network.*
import network.Neuron
import scala.io.Source
import scala.util.Using

final class Mind(
    init: Mind.State,
    neurons: List[Neuron],
    output: Gate,
    mutex: Meter,
    basePrompt: Prompt[AI] = Prompt.empty
):
    private val network = Network.init(neurons, init.maxRecursions)

    def apply[A: Json](input: Input)(using Frame): A < AI =
        apply[A](List(input))

    def apply[A: Json](inputs: List[Input])(using Frame): A < AI =
        val r =
            for
                thoughts <- network(inputs)
                appended =
                    if thoughts.isEmpty then inputs
                    else inputs :+ Input(List("Consider these thoughts"), thoughts)
                out <- output[A](appended)
            yield out
            end for
        end r
        r.handle(
            basePrompt.enable,
            mutex.run,
            Abort.recover(Abort.panic)
        )
    end apply

    export init.name

    def state(using Frame): Mind.State < Async =
        val r =
            for
                neurons <- Kyo.foreach(neurons)(_.state)
                output  <- output.state
            yield init.copy(
                neurons = neurons,
                output = output
            )
        r.handle(
            mutex.run,
            Abort.recover[Closed](Abort.panic)
        )
    end state
end Mind

object Mind:

    case class State(
        name: String,
        @doc("Maximum iterations allowed for recursive neuron activations")
        maxRecursions: Int,
        @doc("Network topology and neuron configurations to generate thoughts")
        neurons: List[Neuron.State],
        @doc("Final output gate that produces the mind's response based on the inputs and network thoughts")
        output: Gate.State,
        @doc("A base prompt to be added as a prefix of all gates of this mind, including neurons")
        basePrompt: String = ""
    ) derives CanEqual, Json:
        require(name.nonEmpty, "Mind name cannot be empty")
        require(maxRecursions >= 0, s"maxRecursions must be non-negative, got $maxRecursions")

        def normalize: State =
            val names = neurons.map(_.name)
            if names.size == names.toSet.size then this
            else
                val renamed =
                    neurons.groupBy(_.name).flatMap { (name, neurons) =>
                        if neurons.size <= 1 then neurons
                        else
                            List(neurons.head) ++ neurons.tail.zipWithIndex.map { (n, idx) =>
                                n.copy(name = name.append(idx.toString))
                            }
                    }
                copy(neurons = List.from(renamed))
            end if
        end normalize

    end State

    def init(state: State)(using Frame): Mind < Sync =
        for
            output  <- network.Gate.init(state.output)
            meter   <- Meter.initMutexUnscoped
            neurons <- Kyo.foreach(state.neurons)(network.Neuron.init(_))
        yield Mind(state, neurons, output, meter, Prompt.init(state.basePrompt))

    def load(path: String)(using Frame): Mind.State < Sync =
        Path(path).read.map(Json.decode[Mind.State](_).getOrThrow)

    def loadDefault(function: String, kind: String, obj: Any, custom: Option[State])(using Frame): State < Sync =
        loadDefault(function, kind, obj.getClass.getSimpleName(), custom)

    def loadDefault(function: String, kind: String, name: String, custom: Option[State])(using Frame): State < Sync =
        custom match
            case Some(state) =>
                state
            case None =>
                for
                    state      <- load(s"$resourcesPath/$function/mind-$kind-$name.json")
                    basePrompt <- Path(s"$resourcesPath/$function/prompt-base.md").read
                yield state.copy(basePrompt = basePrompt)

    private val resourcesPath = "/Users/fwbrasil/workspace/kyo-ai/kyo-ai-ren/shared/src/main/resources"
end Mind
