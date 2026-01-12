package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.network.*

trait Crossover:
    def apply(parent1: Individual, parent2: Individual)(using Frame): Mind.State < AI

object Crossover:

    case class State(
        @doc("Optional custom crossover mind state")
        customMind: Option[Mind.State] = None
    ) derives CanEqual

    def init(state: State)(using Frame): Crossover < Sync =
        Mind.loadDefault("evolution", "crossover", "generic", state.customMind).map { mindState =>
            new Crossover:
                def apply(parent1: Individual, parent2: Individual)(using Frame) =
                    val (mother, father) = if parent1.sex then (parent2, parent1) else (parent1, parent2)

                    for
                        mind <- Mind.init(mindState)
                        plan <- mind[Plan](List(Input(
                            List("Generate the crossover plan for the two genotypes."),
                            List(mother.genotype, father.genotype)
                        )))
                        offspring <- reconstruct(mother.genotype, father.genotype, plan)
                    yield offspring
                    end for
                end apply
        }
    end init

    private[evolution] def reconstruct(
        mother: Mind.State,
        father: Mind.State,
        plan: Plan
    )(using Frame): Mind.State < Sync =
        val motherNeurons = mother.neurons.map(n => n.name -> n).toMap
        val fatherNeurons = father.neurons.map(n => n.name -> n).toMap

        val referencedNeurons = plan.connections.flatMap(c => List(c.from, c.to)).toSet

        val offspringNeurons = referencedNeurons.flatMap { ref =>
            val parent = if ref.sex then motherNeurons else fatherNeurons
            parent.get(ref.neuronName).map { neuron =>
                val newPrevious = plan.connections
                    .filter(_.to == ref)
                    .map(_.from.neuronName)
                    .toList

                val newNext = plan.connections
                    .filter(_.from == ref)
                    .map(_.to.neuronName)
                    .toList

                neuron.copy(
                    previous = newPrevious,
                    next = newNext
                )
            }
        }

        for
            pickMaxRecursions <- Random.nextBoolean
            pickOutput        <- Random.nextBoolean
        yield Mind.State(
            name = plan.name,
            maxRecursions = if pickMaxRecursions then mother.maxRecursions else father.maxRecursions,
            neurons = List.from(offspringNeurons),
            output = if pickOutput then mother.output else father.output,
            basePrompt = mother.basePrompt
        ).normalize
        end for
    end reconstruct

    case class Plan(
        @doc("**Internal** reasoning regarding the generation")
        reasoning: Option[String] = None,
        @doc("Name for the offspring mind. Produce a sensible short and simple name.")
        name: String,
        @doc(
            "Directed edges defining offspring topology. Each edge selects neurons from parents by (mother/father, neuronName). Must only reference neurons that exist in parents."
        )
        connections: List[Connection]
    ) derives CanEqual, Json

    case class Connection(
        from: NeuronRef,
        to: NeuronRef
    ) derives CanEqual, Json

    case class NeuronRef(
        @doc("True to select from mother (female parent), false to select from father (male parent)")
        sex: Boolean,
        @doc("Name of neuron in the specified parent")
        neuronName: Neuron.Name
    ) derives CanEqual, Json

end Crossover
