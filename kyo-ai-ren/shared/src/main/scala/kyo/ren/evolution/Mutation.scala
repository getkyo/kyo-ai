package kyo.ren.evolution

import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.ren.*
import kyo.ren.evolution.Mutation.Strategy
import kyo.ren.network.*
import scala.collection.mutable

// TODO move to a new package and split strategy files
trait Mutation:
    def apply(individual: Individual)(using Frame): Mind.State < AI

object Mutation:

    case class State(
        @doc("Mutation strategies with their relative weights")
        strategies: List[Strategy] = List(
            Strategy.Prompt(45, None),
            Strategy.Structure(35, None),
            Strategy.Params(5, None),
            Strategy.Temp(5),
            Strategy.Repair(5, None),
            Strategy.Subnetwork(3, None),
            Strategy.Memory(2, None)
        ),
        @doc("Probability of applying mutation (0-1)")
        rate: Double = 0.3
    ) derives CanEqual, Json:
        require(strategies.nonEmpty, "Must have at least one mutation strategy")
        require(rate >= 0 && rate <= 1, s"Mutation rate must be in [0, 1], got $rate")
        strategies.foreach { s =>
            require(s.weight > 0, s"Strategy weight must be positive, got ${s.weight}")
        }
    end State

    def init(state: State)(using Frame): Mutation =
        new Mutation:
            def apply(individual: Individual)(using Frame) =
                Random.nextDouble.map(_ < state.rate).map {
                    case true  => Pick(state.strategies, _.weight).map(_(individual))
                    case false => individual.genotype
                }

    sealed abstract class Strategy derives CanEqual, Json:
        def weight: Double
        def apply(individual: Individual)(using Frame): Mind.State < AI

    object Strategy:
        private case class Output[Op](
            @doc("**Internal** reasoning regarding the generation")
            reasoning: Option[String] = None,
            @doc("Make sure to produce cohesive mutation operations that may improve the mind. **Required, must not be empty!**")
            requiredNonEmptyOperationsArray: List[Op]
        ) derives CanEqual, Json

        // TODO fix
        private val modes = List("EXPLORE" -> 0.3, "EXPLOIT" -> 0.7)

        sealed abstract class MindBased[A: Json] extends Strategy:
            def customMind: Option[Mind.State]
            def apply(individual: Individual)(using Frame) =
                for
                    mindState <- Mind.loadDefault("evolution", "mutation", this, customMind)
                    mode      <- Pick(modes, _._2).map(_._1)
                    mind      <- Mind.init(mindState)
                    op <- mind[Output[A]](Input(
                        List(s"Mutate phenotype using mode: $mode. Consider its fitness scores."),
                        List((individual.phenotype, individual.fitness))
                    ))
                yield op.requiredNonEmptyOperationsArray.asInstanceOf[List[Op]].foldLeft(individual.genotype)((g, op) => op(g))
        end MindBased

        case class Structure(weight: Double, customMind: Option[Mind.State])  extends MindBased[Op.StructureOp]
        case class Prompt(weight: Double, customMind: Option[Mind.State])     extends MindBased[Op.PromptOp]
        case class Memory(weight: Double, customMind: Option[Mind.State])     extends MindBased[Op.MemoryOp]
        case class Params(weight: Double, customMind: Option[Mind.State])     extends MindBased[Op.ParamsOp]
        case class Repair(weight: Double, customMind: Option[Mind.State])     extends MindBased[Op]
        case class Subnetwork(weight: Double, customMind: Option[Mind.State]) extends MindBased[Op.StructureOp.AddNeuron]

        case class Temp(weight: Double) extends Strategy:
            def apply(individual: Individual)(using Frame) =
                val genotype = individual.genotype
                for
                    outputGate <-
                        mutate(genotype.output.temperature).map(t =>
                            genotype.output.copy(temperature = t)
                        )

                    neurons <-
                        Kyo.foreach(genotype.neurons) { neuron =>
                            for
                                gateTemp <- mutate(neuron.gate.temperature)

                                memory <-
                                    neuron.memory match
                                        case Some(m) =>
                                            mutate(m.gate.temperature).map(t =>
                                                Some(m.copy(gate = m.gate.copy(temperature = t)))
                                            )
                                        case None => Kyo.lift(None)
                            yield neuron.copy(
                                gate = neuron.gate.copy(temperature = gateTemp),
                                memory = memory
                            )
                        }
                yield genotype.copy(neurons = neurons, output = outputGate)
                end for
            end apply

            private def mutate(temp: Temperature)(using Frame) =
                Random.nextDouble.map {
                    case r if r < 0.7  => temp
                    case r if r < 0.85 => temp.decrease
                    case _             => temp.increase
                }
        end Temp

    end Strategy

    sealed trait Op:
        def apply(genotype: Mind.State): Mind.State

    object Op:

        sealed trait StructureOp:
            def apply(genotype: Mind.State): Mind.State

        object StructureOp:

            case class AddNeuron(
                @doc("Compelling reason to add the neuron.")
                reason: Option[String],
                @doc("Elaborate the characteristics of the new neuron")
                characteristics: Option[String],
                neuron: Neuron.State
            ) extends StructureOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        (genotype.neurons :+ neuron).map { n =>
                            val addToPrev = neuron.next.contains(n.name) && !n.previous.contains(neuron.name)
                            val addToNext = neuron.previous.contains(n.name) && !n.next.contains(neuron.name)

                            if addToPrev || addToNext then
                                n.copy(
                                    previous = if addToPrev then n.previous :+ neuron.name else n.previous,
                                    next = if addToNext then n.next :+ neuron.name else n.next
                                )
                            else n
                            end if
                        }
                    )
            end AddNeuron

            case class RemoveNeuron(
                @doc("Compelling reason to remove the neuron.")
                reason: String,
                neuronName: Neuron.Name
            ) extends StructureOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        genotype.neurons
                            .filter(_.name != neuronName)
                            .map(n =>
                                n.copy(
                                    previous = n.previous.filterNot(_ == neuronName),
                                    next = n.next.filterNot(_ == neuronName)
                                )
                            )
                    )
            end RemoveNeuron

            case class AddConnection(
                @doc("Compelling reason to add the connection.")
                reason: String,
                fromNeuronName: Neuron.Name,
                toNeuronName: Neuron.Name
            ) extends StructureOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        updateNeurons(genotype.neurons, fromNeuronName, toNeuronName)(
                            (n, to) => n.copy(next = if n.next.contains(to) then n.next else n.next :+ to),
                            (n, from) => n.copy(previous = if n.previous.contains(from) then n.previous else n.previous :+ from)
                        )
                    )
            end AddConnection

            case class RemoveConnection(
                @doc("Compelling reason to remove the connection.")
                reason: String,
                fromNeuronName: Neuron.Name,
                toNeuronName: Neuron.Name
            ) extends StructureOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        updateNeurons(genotype.neurons, fromNeuronName, toNeuronName)(
                            (n, to) => n.copy(next = n.next.filterNot(_ == to)),
                            (n, from) => n.copy(previous = n.previous.filterNot(_ == from))
                        )
                    )
            end RemoveConnection
        end StructureOp

        sealed trait PromptOp:
            def apply(genotype: Mind.State): Mind.State

        object PromptOp:

            case class MindOutput(
                @doc("Compelling reason to change the mind's final generation output gate.")
                reason: String,
                @doc("Elaborate the characteristics of the new prompt")
                characteristics: Option[String],
                prompt: String,
                reminder: String
            ) extends PromptOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(output = genotype.output.copy(prompt = prompt, reminder = reminder))
            end MindOutput

            case class NeuronOutput(
                @doc("Compelling reason to change the neuron's gate prompt.")
                reason: String,
                @doc("Elaborate the characteristics of the new prompt")
                characteristics: Option[String],
                neuronName: Neuron.Name,
                prompt: String,
                reminder: String
            ) extends PromptOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        updateNeuron(genotype.neurons, neuronName)(n =>
                            n.copy(gate = n.gate.copy(prompt = prompt, reminder = reminder))
                        )
                    )
            end NeuronOutput

            case class NeuronMemory(
                @doc("Compelling reason to change the neuron's memory gate prompt.")
                reason: String,
                @doc("Elaborate the characteristics of the new prompt")
                characteristics: Option[String],
                neuronName: Neuron.Name,
                prompt: String,
                reminder: String
            ) extends PromptOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        updateNeuron(genotype.neurons, neuronName)(n =>
                            n.copy(memory =
                                n.memory.map(m =>
                                    m.copy(gate = m.gate.copy(prompt = prompt, reminder = reminder))
                                )
                            )
                        )
                    )
            end NeuronMemory
        end PromptOp

        sealed trait MemoryOp:
            def apply(genotype: Mind.State): Mind.State =
                val (name, updater) = this match
                    case MemoryOp.UpsertMemory(_, _, n, memory) => (n, (neuron: Neuron.State) => neuron.copy(memory = Some(memory)))
                    case MemoryOp.RemoveMemory(_, n)            => (n, (neuron: Neuron.State) => neuron.copy(memory = None))

                genotype.copy(neurons = updateNeuron(genotype.neurons, name)(updater))
            end apply
        end MemoryOp

        object MemoryOp:
            case class UpsertMemory(
                @doc("Compelling reason to usert the memory.")
                reason: String,
                characteristics: Option[String],
                neuronName: Neuron.Name,
                memory: Memory.State
            ) extends MemoryOp with Op

            case class RemoveMemory(
                @doc("Compelling reason to remove the memory.")
                reason: String,
                neuronName: Neuron.Name
            ) extends MemoryOp with Op
        end MemoryOp

        sealed trait ParamsOp:
            def apply(genotype: Mind.State): Mind.State

        object ParamsOp:
            case class MindParams(
                @doc("Compelling reason to update the mind's params")
                reason: String,
                maxRecursions: Int,
                outputHistorySize: Int
            ) extends ParamsOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(
                        maxRecursions = maxRecursions,
                        output = updateHistory(genotype.output, outputHistorySize)
                    )
            end MindParams

            case class NeuronParams(
                @doc("Compelling reason to update the neurons's params")
                reason: String,
                neuronName: Neuron.Name,
                conscious: Boolean,
                outputHistorySize: Option[Int],
                memoryHistorySize: Option[Int]
            ) extends ParamsOp with Op:
                def apply(genotype: Mind.State) =
                    genotype.copy(neurons =
                        updateNeuron(genotype.neurons, neuronName) { n =>
                            n.copy(
                                conscious = conscious,
                                gate = updateHistory(n.gate, outputHistorySize.getOrElse(n.gate.history.maxEntries)),
                                memory = n.memory.flatMap(m =>
                                    memoryHistorySize.map(h => m.copy(gate = updateHistory(m.gate, h)))
                                ).orElse(n.memory)
                            )
                        }
                    )
            end NeuronParams
        end ParamsOp

        private[Op] def updateNeuron(
            neurons: List[Neuron.State],
            name: Neuron.Name
        )(f: Neuron.State => Neuron.State): List[Neuron.State] =
            neurons.map(n => if n.name == name then f(n) else n)

        private[Op] def updateNeurons(
            neurons: List[Neuron.State],
            from: Neuron.Name,
            to: Neuron.Name
        )(
            updateFrom: (Neuron.State, Neuron.Name) => Neuron.State,
            updateTo: (Neuron.State, Neuron.Name) => Neuron.State
        ): List[Neuron.State] =
            neurons.map { n =>
                if n.name == from then updateFrom(n, to)
                else if n.name == to then updateTo(n, from)
                else n
            }

        private[Op] def updateHistory(
            gate: Gate.State,
            size: Int
        ): Gate.State =
            gate.copy(history = gate.history.copy(maxEntries = size))

    end Op

end Mutation
