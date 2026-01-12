package kyo.ren.network

import kyo.*
import kyo.ai.json.*
import kyo.ai.json.doc

final class Neuron(
    val init: Neuron.State,
    memory: Option[Memory],
    gate: Gate
):
    def apply(inputs: List[Input], reachable: List[Neuron])(using Frame): Neuron.Output < AI =
        val withMemory =
            memory match
                case None => Kyo.lift(inputs)
                case Some(memory) =>
                    memory(inputs).map { entries =>
                        inputs :+ Input(
                            List("Consider the recovered memory entries"),
                            List(Memory.toMarkdown(entries))
                        )
                    }

        withMemory.map { inputs =>
            val appended =
                inputs :+
                    Input(
                        List("Consider the valid next neurons to select for activation."),
                        List(Neuron.toMarkdown(reachable))
                    )
            init.conscious match
                case true =>
                    gate[Neuron.Output.Conscious](appended)
                case flase =>
                    gate[Neuron.Output.Subconscious](appended)
            end match
        }
    end apply

    export init.conscious
    export init.name as name
    export init.next
    export init.previous

    def state(using Frame): Neuron.State < Sync =
        for
            memoryState <- memory.map(_.state.map(Some(_))).getOrElse(Kyo.lift(None))
            gateState   <- gate.state
        yield Sync.defer {
            init.copy(
                memory = memoryState,
                gate = gateState
            )
        }
end Neuron

object Neuron:

    opaque type Name <: String = String

    object Name:
        given Json[Name]           = Json[String]
        given CanEqual[Name, Name] = CanEqual.derived

        def apply(s: String): Name = s

        extension (self: Name)
            def append(s: String): Name = self + s
    end Name

    case class State(
        name: Name,
        description: String,
        @doc("Whether this neuron's output should be collected as a consicous thought by the mind.")
        conscious: Boolean,
        previous: List[Name],
        next: List[Name],
        @doc("Optional memory component for stateful processing")
        memory: Option[Memory.State],
        @doc("Gate that processes inputs and network thoughts (conscious=true) to produce the final mind output")
        gate: Gate.State
    ) derives CanEqual, Json:
        require(name.nonEmpty, "Neuron name cannot be empty")
        require(description.nonEmpty, "Neuron description cannot be empty")
    end State

    def init(state: State)(using Frame): Neuron < Sync =
        for
            memory <- state.memory.map(Memory.init(_).map(Some(_))).getOrElse(Kyo.lift(None))
            gate   <- Gate.init(state.gate)
        yield Neuron(state, memory, gate)

    case class Activation(
        @doc("Valid next neurons to deliver this activation")
        neuronNames: List[Neuron.Name],
        @doc("Description of the tasks to be executed")
        tasks: Option[List[String]],
        @doc("Contextual information to perform the tasks")
        context: Option[List[String]]
    ) derives CanEqual, Json

    sealed trait Output:
        def output: Option[String]
        def activations: Option[List[Activation]]

    object Output:

        @doc("You now have an opportunity to produce a conscious thought to be presented in the final generation. " +
            "Leverage this opportunity to provide a comprehensive knowledge transfer of valuable insights.")
        case class Conscious(
            @doc("The required conscious output to be included in the final generation. Leverage the opportunity.")
            requiredConsciousReasoning: String,
            @doc("Valid next neurons to fire. Each activation must be self-contained and include all " +
                "necessary contextual information to perform the tasks.")
            activations: Option[List[Activation]]
        ) extends Output derives CanEqual, Json:
            def output = Some(requiredConsciousReasoning)
        end Conscious

        case class Subconscious(
            @doc("**Internal** reasoning regarding the generation")
            reasoning: Option[String] = None,
            @doc("Valid next neurons to fire")
            activations: Option[List[Activation]]
        ) extends Output derives CanEqual, Json:
            def output = None
        end Subconscious
    end Output

    def toMarkdown(neurons: List[Neuron]): String =
        if neurons.isEmpty then "## Neurons [name] description\n\nEmpty"
        else
            val entries = neurons.map { neuron =>
                s"### [${neuron.init.name}]\n\n${neuron.init.description}"
            }.mkString("\n\n")
            s"## Neurons [name] description\n\n$entries"
    end toMarkdown

end Neuron
