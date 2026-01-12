package kyo.ren.evolution

import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc

trait Selection:
    def sample(individuals: List[Individual], count: Int)(using Frame): List[Individual] < Sync

object Selection:

    enum State derives CanEqual, Json:
        @doc("Multi-objective optimization selecting non-dominated solutions") case Pareto
        @doc("Simple fitness-based selection of top performers") case Fitness

    def init(state: State)(using Frame): Selection =
        state match
            case State.Pareto  => pareto
            case State.Fitness => fitness

    private val fitness = new Selection:
        def sample(individuals: List[Individual], count: Int)(using Frame) =
            if individuals.isEmpty || count <= 0 then
                Kyo.lift(List.empty)
            else
                Kyo.lift(individuals.sortBy(-_.fitness.map(_.score).sum).take(count))

    private val pareto = new Selection:
        def sample(individuals: List[Individual], count: Int)(using Frame) =
            if individuals.isEmpty || count <= 0 then List.empty
            else
                val front = frontier(individuals)
                if front.size >= count then
                    val sorted = front.sortBy(ind => -coverage(ind, front))
                    sorted.take(count)
                else
                    val remaining  = individuals.filterNot(front.contains)
                    val additional = remaining.sortBy(-_.fitness.map(_.score).sum).take(count - front.size)
                    front ++ additional
                end if

        private def frontier(individuals: List[Individual]): List[Individual] =
            val testCount = individuals.head.fitness.size

            val instanceSets = (0 until testCount).map { testIdx =>
                val maxScore = individuals.map(_.fitness(testIdx)).map(_.score).max
                individuals.filter(_.fitness(testIdx).score == maxScore)
            }

            val candidates = instanceSets.flatten.toSet

            List.from(candidates).filterNot { a =>
                candidates.exists { b =>
                    b != a && dominates(b, a, individuals)
                }
            }
        end frontier

        private def coverage(individual: Individual, all: List[Individual]): Int =
            (0 until individual.fitness.size).count(isBestAt(individual, _, all))

        private def dominates(a: Individual, b: Individual, all: List[Individual]): Boolean =
            val testCount = a.fitness.size
            val aBest     = (0 until testCount).filter(isBestAt(a, _, all)).toSet
            val bBest     = (0 until testCount).filter(isBestAt(b, _, all)).toSet
            bBest.subsetOf(aBest) && aBest != bBest
        end dominates

        private def isBestAt(individual: Individual, testIdx: Int, all: List[Individual]): Boolean =
            individual.fitness(testIdx).score == all.map(_.fitness(testIdx)).map(_.score).max

end Selection
