package kyo.ren.network

import Neuron.Activation
import kyo.*
import kyo.ai.json.Json
import kyo.ren.network.Network.FlatActivation

final class Network(
    neurons: List[Neuron],
    maxIterations: Int
):

    private val all = neurons.map(n => n.name -> n).toMap

    private val canReachConscious =
        val conscious = neurons.filter(_.conscious).map(_.name).toSet
        val reverseEdges =
            neurons.flatMap(n => n.next.map(_ -> n.name))
                .groupBy(_._1)
                .view.mapValues(_.map(_._2).toSet)

        @annotation.tailrec
        def reachable(frontier: Set[Neuron.Name], visited: Set[Neuron.Name]): Set[Neuron.Name] =
            if frontier.isEmpty then visited
            else
                val next = frontier.flatMap(reverseEdges.getOrElse(_, Set.empty)) -- visited
                reachable(next, visited ++ frontier)

        reachable(conscious, Set.empty)
    end canReachConscious

    private val roots =
        neurons.filter(_.previous.isEmpty)

    private val edges =
        neurons.map { n =>
            n.name -> neurons.filter { other =>
                n.next.contains(other.name) &&
                other.previous.contains(n.name)
            }
        }.toMap

    def apply(inputs: List[Input])(using Frame): List[String] < AI =
        def loop(
            iterNum: Int,
            activations: List[FlatActivation],
            acc: List[String],
            seenThisIteration: Set[Neuron.Name],
            globalSeen: Map[Neuron.Name, Int]
        ): List[String] < AI =
            if iterNum >= maxIterations then acc
            else
                def isValid(name: Neuron.Name) =
                    !seenThisIteration.contains(name) &&
                        globalSeen.getOrElse(name, 0) < maxIterations &&
                        all.contains(name) &&
                        canReachConscious.contains(name)

                val valid = activations.filter(a => isValid(a.neuronName))
                if valid.isEmpty then acc
                else
                    Async.foreach(valid.groupBy(_.neuronName).toSeq) { (name, activations) =>
                        all(name)(
                            activations.map(a => Input(a.tasks, a.context, a.from)),
                            edges.getOrElse(name, List.empty).filter(n => isValid(n.name))
                        ).map { results =>
                            (
                                name,
                                results.output,
                                results.activations.getOrElse(List.empty).flatMap { a =>
                                    a.neuronNames.map(n =>
                                        FlatActivation(
                                            Some(name),
                                            n,
                                            a.tasks.getOrElse(List.empty),
                                            a.context.getOrElse(List.empty)
                                        )
                                    )
                                }
                            )
                        }
                    }.map { results =>

                        val collected = results.collect {
                            case (name, output, _) if all(name).conscious && output.nonEmpty => output
                        }.flatten

                        val newActivations = results.flatMap { case (sourceName, _, activations) =>
                            activations.filter(a => edges.getOrElse(sourceName, List.empty).exists(_.name == a.neuronName))
                        }

                        val hasRecursion = newActivations.exists(a => seenThisIteration.contains(a.neuronName))

                        val updatedGlobal = valid.foldLeft(globalSeen) { (seen, a) =>
                            seen.updated(a.neuronName, seen.getOrElse(a.neuronName, 0) + 1)
                        }

                        val nextSeen = if hasRecursion then Set.empty else seenThisIteration ++ valid.map(_.neuronName)

                        loop(
                            iterNum + 1,
                            newActivations.toList,
                            acc ++ collected,
                            nextSeen,
                            updatedGlobal
                        )
                    }
                end if
        end loop

        val init =
            if inputs.isEmpty then
                roots.map(root => FlatActivation(None, root.name, Nil, Nil))
            else
                for
                    root  <- roots
                    input <- inputs
                yield FlatActivation(None, root.name, input.tasks, input.context)

        loop(0, init, List.empty, Set.empty, Map.empty)
    end apply
end Network

object Network:

    private[network] case class FlatActivation(
        from: Option[Neuron.Name],
        neuronName: Neuron.Name,
        tasks: List[String],
        context: List[String]
    )

    def init(neurons: List[Neuron], maxIterations: Int): Network =
        new Network(neurons, maxIterations)
end Network
