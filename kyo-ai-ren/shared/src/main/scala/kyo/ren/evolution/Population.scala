package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.*
import kyo.ren.network.*

final class Population(
    init: Population.State
):
    private var individuals = init.individuals
    private var generation  = init.generation

    def eval(
        tests: List[Evolution.TestCase],
        fitness: Fitness
    )(using Frame): Unit < AI =
        if individuals.exists(_.fitness.nonEmpty) then ()
        else
            Queue.Unbounded.use[(Individual, TestCase)]() { queue =>
                for
                    selectedTests <-
                        Random.shuffle(tests).map(_.take(init.maxTestsByIteration))

                    _ <- Kyo.foreach(selectedTests)(t => Kyo.foreach(individuals)(i => queue.add(i -> t)))

                    evaluated <-
                        Async.fill(100)(
                            Loop[Chunk[(Individual, Mind.State, TestScore)], Chunk[(Individual, Mind.State, TestScore)], AI](Chunk.empty) {
                                acc =>
                                    queue.poll.map {
                                        case Present((individual, test)) =>
                                            Retry[Throwable] {
                                                for
                                                    mind      <- Mind.init(individual.genotype)
                                                    score     <- fitness(mind, test)
                                                    phenotype <- mind.state
                                                yield (individual, phenotype, TestScore(test.testId, score))
                                            }.map { r =>
                                                Loop.continue(acc.append(r))
                                            }
                                        case Absent =>
                                            Loop.done[Chunk[(Individual, Mind.State, TestScore)], Chunk[(
                                                Individual,
                                                Mind.State,
                                                TestScore
                                            )]](acc)
                                    }.handle(
                                        Abort.recover[Throwable](Abort.panic)
                                    )
                            }
                        ).map(_.flattenChunk)

                    empty <- queue.empty.handle(Abort.recoverError(_ => false))
                yield
                    require(empty, "Queue should be empty!")
                    Kyo.foreach(evaluated.groupBy(_._1).toSeq) {
                        case (individual, results) =>
                            val sorted = results.sortBy(_._3.score)
                            Random.nextBoolean.map {
                                case true  => sorted.head._2
                                case false => sorted.last._2
                            }.map { phenotype =>
                                individual.copy(phenotype = phenotype, fitness = results.map(_._3).toList)
                            }
                    }.map { evaluated =>
                        Sync.defer {
                            individuals = evaluated.toList
                        }
                    }
                end for
            }
    end eval

    def breed(
        eliteFraction: Double,
        mutation: Mutation,
        crossover: Crossover,
        selection: Selection
    )(using Frame): Unit < AI =
        if individuals.exists(_.fitness.isEmpty) then ()
        else
            val currentSize = individuals.size.min(init.maxSize)
            val eliteCount =
                if currentSize < init.maxSize then currentSize
                else math.max(2, (currentSize * eliteFraction).ceil.toInt)

            val targetSize =
                if currentSize < init.maxSize then
                    val grown = (currentSize * init.growthFactor).ceil.toInt
                    math.max(currentSize + 1, grown).min(init.maxSize)
                else
                    currentSize

            val offspringCount = targetSize - eliteCount

            for
                elites <- selection.sample(individuals, eliteCount)
                pairs  <- Individual.breedingPairs(individuals, init.mating, offspringCount)
                offspring <-
                    Async.foreach(pairs) { case (parent1, parent2) =>
                        for
                            parent1Genotype <- mutation(parent1)
                            parent2Genotype <- mutation(parent2)
                            child <- crossover(
                                parent1.copy(genotype = parent1Genotype, phenotype = parent1Genotype),
                                parent2.copy(genotype = parent2Genotype, phenotype = parent2Genotype)
                            )
                        yield Individual.init(parent1, parent2, child)
                    }

                replicated <-
                    if offspring.size < offspringCount then
                        val needed = offspringCount - offspring.size
                        Async.foreach(List.fill(needed)(elites), 5) { eliteChunk =>
                            for
                                parent <- Random.shuffle(eliteChunk).map(_.head)
                                child  <- mutation(parent)
                            yield Individual.init(parent, parent, child)
                        }
                    else
                        Kyo.lift(List.empty)
            yield Sync.defer {
                individuals = (elites ++ offspring ++ replicated).map(_.copy(fitness = List.empty))
                generation = generation + 1
            }
            end for
    end breed

    def best: Individual         = individuals.maxBy(_.fitness.map(_.score).sum)
    def ranked: List[Individual] = individuals.sortBy(-_.fitness.map(_.score).sum)
    def size: Int                = individuals.size
    def gen: Int                 = generation

    def state(using Frame): Population.State < Sync =
        Sync.defer(init.copy(individuals = individuals, generation = generation))

end Population

object Population:

    case class State(
        individuals: List[Individual],
        maxSize: Int,
        @doc("Factor by which population grows each generation (must be >= 1.0)")
        growthFactor: Double,
        @doc("Maximum number of test cases to evaluate per iteration")
        maxTestsByIteration: Int = Int.MaxValue,
        @doc("Weights for sexual selection mate preferences")
        mating: Mating = Mating(),
        generation: Int = 0
    ) derives CanEqual, Json:
        require(individuals.nonEmpty, "Population must have at least one individual")
        require(generation >= 0, s"Generation must be non-negative, got $generation")
        require(maxSize > 0, s"maxSize must be positive, got $maxSize")
        // require(individuals.size <= maxSize, s"Population size ${individuals.size} exceeds maxSize $maxSize")
        require(growthFactor >= 1.0, s"growthFactor must be >= 1.0, got $growthFactor")
    end State

    def init(state: State)(using Frame): Population < Sync =
        Sync.defer(Population(state))

    def seed(
        initial: Mind.State,
        size: Int,
        customSeeder: Option[Mind.State]
    )(using Frame): List[Individual] < AI =
        for
            seederState <- Mind.loadDefault("evolution", "population", "seeder", customSeeder)
            seeder      <- Mind.init(seederState)
            genotypes   <- Kyo.fill(size)(seeder[Mind.State](List(Input(initial))))
        yield genotypes.zipWithIndex.map { (genotype, idx) =>
            Individual(
                genotype = genotype,
                phenotype = genotype,
                fitness = List.empty,
                sex = idx % 2 == 0,
                lineage = Lineage.empty
            )
        }.toList

    case class Mating(
        @doc("Weight for genetic distance - avoiding inbreeding")
        geneticDistance: Double = 0.3,
        @doc("Weight for complementarity - covering weaknesses")
        complementarity: Double = 0.6,
        @doc("Weight for similarity - compatible approaches")
        similarity: Double = 0.1,
        @doc("Tournament size for selecting parents")
        tournamentSize: Int = 3
    ) derives CanEqual, Json:
        require(tournamentSize > 0, s"tournamentSize must be positive, got $tournamentSize")

        def normalized: Mating =
            val total = geneticDistance + complementarity + similarity
            if total == 0.0 then Mating()
            else if total == 1.0 then this
            else
                copy(
                    geneticDistance = geneticDistance / total,
                    complementarity = complementarity / total,
                    similarity = similarity / total
                )
            end if
        end normalized
    end Mating

end Population
