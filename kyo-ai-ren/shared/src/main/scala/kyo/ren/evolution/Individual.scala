package kyo.ren.evolution

import Population.Mating
import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.ren.*
import kyo.ren.evolution.Evolution.*
import scala.annotation.tailrec

case class Individual(
    genotype: Mind.State,
    phenotype: Mind.State,
    fitness: List[TestScore],
    sex: Boolean,
    lineage: Lineage
) derives CanEqual, Json:

    private[evolution] def selectMates(candidates: List[Individual], weights: Mating): List[(Individual, Double)] =
        candidates
            .filter(other => sex != other.sex && this.genotype != other.genotype)
            .map(other => other -> attractionTo(other, weights.normalized))
            .filter(_._2 > 0)
            .sortBy(-_._2)

    private[evolution] def attractionTo(other: Individual, weights: Mating): Double =
        if sex == other.sex || this == other then 0
        else
            val geneticDistance = this.geneticDistance(other)
            val complementarity = this.complementarity(other)
            val similarity      = this.similarity(other)
            geneticDistance * weights.geneticDistance +
                complementarity * weights.complementarity +
                similarity * weights.similarity

    private[evolution] def geneticDistance(other: Individual): Double =
        lineage.distanceTo(other.lineage)

    private[evolution] def complementarity(other: Individual): Double =
        if fitness.isEmpty then 0.5
        else
            val gapFilling =
                fitness.zip(other.fitness).map { case (myScore, theirScore) =>
                    val myGap = 1.0 - myScore.score
                    theirScore.score * myGap
                }.sum
            gapFilling / fitness.size

    private[evolution] def similarity(other: Individual): Double =
        val g1 = genotype
        val g2 = other.genotype

        if g1.neurons.isEmpty || g2.neurons.isEmpty then 0.5
        else
            val neuronCountSim =
                math.abs(g1.neurons.size - g2.neurons.size).toDouble /
                    math.max(g1.neurons.size, g2.neurons.size)

            def avgConnections(g: Mind.State) =
                g.neurons.map(n => n.next.size + n.previous.size).sum.toDouble / g.neurons.size
            val topologySim =
                math.abs(avgConnections(g1) - avgConnections(g2)) /
                    math.max(avgConnections(g1), avgConnections(g2)).max(1.0)

            def consciousRatio(g: Mind.State) =
                g.neurons.count(_.conscious).toDouble / g.neurons.size
            val consciousSim =
                math.abs(consciousRatio(g1) - consciousRatio(g2))

            def memoryRatio(g: Mind.State) =
                g.neurons.count(_.memory.isDefined).toDouble / g.neurons.size
            val memorySim =
                math.abs(memoryRatio(g1) - memoryRatio(g2))

            val sharedNeuronDist     = stringSetDistance(g1.neurons.map(_.name).toSet, g2.neurons.map(_.name).toSet)
            val neuronGatePromptDist = stringSetDistance(g1.neurons.map(_.gate.prompt).toSet, g2.neurons.map(_.gate.prompt).toSet)
            val memoryPromptDist = stringSetDistance(
                g1.neurons.flatMap(_.memory.map(_.gate.prompt)).toSet,
                g2.neurons.flatMap(_.memory.map(_.gate.prompt)).toSet
            )
            val mindOutputPromptDist = stringSetDistance(Set(g1.output.prompt), Set(g2.output.prompt))

            val scores = List(
                neuronCountSim,
                topologySim,
                consciousSim,
                memorySim,
                sharedNeuronDist,
                neuronGatePromptDist,
                memoryPromptDist,
                mindOutputPromptDist
            )

            val size = scores.size.toDouble
            (size - scores.sum) / size
        end if
    end similarity

    private def stringSetDistance(set1: Set[String], set2: Set[String]): Double =
        val sharedCount = set1.intersect(set2).size
        val totalUnique = set1.union(set2).size
        if totalUnique == 0 then 0.0 else 1.0 - (sharedCount.toDouble / totalUnique)
    end stringSetDistance

end Individual

object Individual:

    def init(parent1: Individual, parent2: Individual, child: Mind.State)(using Frame): Individual < Sync =
        Random.nextBoolean.map { childSex =>
            Individual(
                genotype = child,
                phenotype = child,
                fitness = List.empty,
                sex = childSex,
                lineage = Lineage.init(parent1, parent2)
            )
        }

    def breedingPairs(
        individuals: List[Individual],
        weights: Mating,
        targetPairs: Int
    )(using Frame): List[(Individual, Individual)] < Sync =
        val (females, males) = individuals.partition(!_.sex)
        if females.isEmpty || males.isEmpty then List.empty
        else
            val w = weights.normalized
            Kyo.fill(targetPairs) {
                for
                    females <- Random.shuffle(females).map(_.take(w.tournamentSize.min(males.size)))
                    males   <- Random.shuffle(males).map(_.take(w.tournamentSize.min(females.size)))
                yield
                    val female   = females.maxBy(_.fitness.map(_._2).sum)
                    val selected = female.selectMates(List.from(males), w)
                    if selected.isEmpty then None
                    else
                        Pick(selected, _._2).map(_._1).map(Some(female, _))
                    end if
            }.map(_.flatten.toList)
        end if
    end breedingPairs

end Individual
