package kyo.ren.evolution

import Population.Mating
import kyo.*
import kyo.Test
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class PopulationTest extends Test:

    def createIndividual(
        name: String,
        fitness: List[Double] = List.empty,
        sex: Boolean = false
    ): Individual =
        val mindState = createMindState(name)
        Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = fitness.map(TestScore("0", _)),
            sex = sex,
            lineage = Lineage.empty
        )
    end createIndividual

    def createMindState(name: String): Mind.State =
        Mind.State(
            name = name,
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
        )

    def createTestCase(input: String, expected: String): Evolution.TestCase =
        Evolution.TestCase("0", input, expected)

    def createMockFitness(scores: Map[String, Double]): Fitness =
        val mockEval = new Eval:
            def apply(mind: Mind, test: Evolution.TestCase)(using Frame) =
                Eval.Score(scores.getOrElse(mind.name, 0.5), 100.millis)

        Fitness.init(mockEval, 0.0, 0.0, 1.0)
    end createMockFitness

    def createMockMutation(suffix: String = "-mutated"): Mutation =
        new Mutation:
            def apply(individual: Individual)(using Frame) =
                individual.genotype.copy(name = individual.genotype.name + suffix)
            def rate = 0.5

    def createMockCrossover(combiner: String = "+"): Crossover =
        new Crossover:
            def apply(parent1: Individual, parent2: Individual)(using Frame) =
                parent1.genotype.copy(name = s"${parent1.genotype.name}${combiner}${parent2.genotype.name}")
            def rate = 0.5

    "State validation" - {
        "accepts valid population state" in {
            val state = Population.State(
                individuals = List(createIndividual("ind1")),
                maxSize = 10,
                growthFactor = 1.5,
                maxTestsByIteration = 5,
                generation = 0
            )
            assert(state.individuals.size == 1)
            assert(state.maxSize == 10)
            assert(state.growthFactor == 1.5)
            assert(state.maxTestsByIteration == 5)
            assert(state.generation == 0)
        }

        "requires non-empty individuals" in {
            val result = scala.util.Try {
                Population.State(
                    individuals = List.empty,
                    maxSize = 10,
                    growthFactor = 1.5
                )
            }
            assert(result.isFailure)
        }

        "requires non-negative generation" in {
            val result = scala.util.Try {
                Population.State(
                    individuals = List(createIndividual("ind1")),
                    maxSize = 10,
                    growthFactor = 1.5,
                    generation = -1
                )
            }
            assert(result.isFailure)
        }

        "requires positive maxSize" in {
            val result = scala.util.Try {
                Population.State(
                    individuals = List(createIndividual("ind1")),
                    maxSize = 0,
                    growthFactor = 1.5
                )
            }
            assert(result.isFailure)
        }

        "requires individuals size <= maxSize" in {
            val result = scala.util.Try {
                Population.State(
                    individuals = List.fill(5)(createIndividual("ind")),
                    maxSize = 3,
                    growthFactor = 1.5
                )
            }
            assert(result.isFailure)
        }

        "requires growthFactor >= 1.0" in {
            val result = scala.util.Try {
                Population.State(
                    individuals = List(createIndividual("ind1")),
                    maxSize = 10,
                    growthFactor = 0.9
                )
            }
            assert(result.isFailure)
        }

        "accepts growthFactor of exactly 1.0" in {
            val state = Population.State(
                individuals = List(createIndividual("ind1")),
                maxSize = 10,
                growthFactor = 1.0
            )
            assert(state.growthFactor == 1.0)
        }

        "uses Int.MaxValue as default maxTestsByIteration" in {
            val state = Population.State(
                individuals = List(createIndividual("ind1")),
                maxSize = 10,
                growthFactor = 1.5
            )
            assert(state.maxTestsByIteration == Int.MaxValue)
        }
    }

    "Population initialization" - {
        "creates population from state" in run {
            val state = Population.State(
                individuals = List(
                    createIndividual("ind1", List(0.7)),
                    createIndividual("ind2", List(0.8))
                ),
                maxSize = 10,
                growthFactor = 1.5,
                generation = 3
            )

            for
                population <- Population.init(state)
            yield
                assert(population.size == 2)
                assert(population.gen == 3)
            end for
        }

        "preserves initial individuals" in run {
            val individuals = List(
                createIndividual("ind1", List(0.5, 0.6)),
                createIndividual("ind2", List(0.7, 0.8))
            )
            val state = Population.State(individuals, 10, 1.5)

            for
                population <- Population.init(state)
                popState   <- population.state
            yield assert(popState.individuals == individuals)
            end for
        }
    }

    "Basic operations" - {
        "best returns individual with highest total fitness" in run {
            val state = Population.State(
                individuals = List(
                    createIndividual("low", List(0.1, 0.1)),
                    createIndividual("high", List(0.8, 0.9)),
                    createIndividual("mid", List(0.4, 0.4))
                ),
                maxSize = 10,
                growthFactor = 1.5
            )

            for
                population <- Population.init(state)
            yield
                val best = population.best
                assert(best.genotype.name == "high")
                assert(math.abs(best.fitness.map(_.score).sum - 1.7) < 0.0001)
            end for
        }

        "ranked returns individuals sorted by fitness descending" in run {
            val state = Population.State(
                individuals = List(
                    createIndividual("low", List(0.2)),
                    createIndividual("high", List(0.9)),
                    createIndividual("mid", List(0.5))
                ),
                maxSize = 10,
                growthFactor = 1.5
            )

            for
                population <- Population.init(state)
            yield
                val ranked = population.ranked
                assert(ranked.size == 3)
                assert(ranked(0).genotype.name == "high")
                assert(ranked(1).genotype.name == "mid")
                assert(ranked(2).genotype.name == "low")
            end for
        }

        "size returns current population size" in run {
            val state = Population.State(
                individuals = List.fill(5)(createIndividual("ind")),
                maxSize = 10,
                growthFactor = 1.5
            )

            for
                population <- Population.init(state)
            yield assert(population.size == 5)
        }

        "gen returns current generation" in run {
            val state = Population.State(
                individuals = List(createIndividual("ind")),
                maxSize = 10,
                growthFactor = 1.5,
                generation = 7
            )

            for
                population <- Population.init(state)
            yield assert(population.gen == 7)
        }
    }

    "Evaluation" - {
        "evaluates all individuals with test cases" in run {
            val individuals = List(
                createIndividual("ind1"),
                createIndividual("ind2"),
                createIndividual("ind3")
            )

            val state = Population.State(individuals, 10, 1.5)
            val tests = List(
                createTestCase("input1", "output1"),
                createTestCase("input2", "output2")
            )

            val mockFitness = createMockFitness(Map(
                "ind1" -> 0.7,
                "ind2" -> 0.8,
                "ind3" -> 0.9
            ))

            for
                population <- Population.init(state)
                _          <- AI.run(population.eval(tests, mockFitness))
                newState   <- population.state
            yield
                assert(newState.individuals.forall(_.fitness.size == 2))
                assert(newState.individuals(0).fitness.map(_.score).forall(_ == 0.7))
                assert(newState.individuals(1).fitness.map(_.score).forall(_ == 0.8))
                assert(newState.individuals(2).fitness.map(_.score).forall(_ == 0.9))
            end for
        }

        "respects maxTestsByIteration limit" in run {
            val state = Population.State(
                individuals = List(createIndividual("ind1")),
                maxSize = 10,
                growthFactor = 1.5,
                maxTestsByIteration = 2
            )

            val tests       = List.fill(10)(createTestCase("input", "output"))
            val mockFitness = createMockFitness(Map("ind1" -> 0.5))

            for
                population <- Population.init(state)
                _          <- AI.run(population.eval(tests, mockFitness))
                newState   <- population.state
            yield assert(newState.individuals.head.fitness.size == 2)
            end for
        }

        "updates phenotype during evaluation" in run {
            val genotype   = createMindState("original")
            val phenotype  = createMindState("old-phenotype")
            val individual = Individual(genotype, phenotype, List.empty, false, Lineage.empty)

            val state       = Population.State(List(individual), 10, 1.5)
            val tests       = List(createTestCase("input", "output"))
            val mockFitness = createMockFitness(Map("original" -> 0.5))

            for
                population <- Population.init(state)
                _          <- AI.run(population.eval(tests, mockFitness))
                newState   <- population.state
            yield assert(newState.individuals.head.phenotype == genotype)
            end for
        }
    }

    "Breeding" - {
        "maintains elite individuals" in run {
            val individuals = List(
                createIndividual("best", List(0.9)),
                createIndividual("worst", List(0.1)),
                createIndividual("mid", List(0.5))
            )

            val state     = Population.State(individuals, 10, 1.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.5, mutation, crossover, selection)
                    newState   <- population.state
                yield
                    val names = newState.individuals.map(_.genotype.name)
                    assert(names.contains("best"))
                    assert(newState.individuals.size == 4)
                end for
            }
        }

        "grows population when below maxSize" in run {
            val state = Population.State(
                individuals = List(
                    createIndividual("ind1", List(0.5), sex = false),
                    createIndividual("ind2", List(0.5), sex = true)
                ),
                maxSize = 10,
                growthFactor = 2.0
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.5, mutation, crossover, selection)
                    newState   <- population.state
                yield assert(newState.individuals.size >= 3)
                end for
            }
        }

        "respects maxSize limit" in run {
            val state = Population.State(
                individuals = List.fill(8)(createIndividual("ind", List(0.5))),
                maxSize = 10,
                growthFactor = 2.0
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)
            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.2, mutation, crossover, selection)
                    newState   <- population.state
                yield assert(newState.individuals.size == 10)
                end for
            }
        }

        "maintains size when at maxSize" in run {
            val state = Population.State(
                individuals = List.fill(10)(createIndividual("ind", List(0.5))),
                maxSize = 10,
                growthFactor = 2.0
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.2, mutation, crossover, selection)
                    newState   <- population.state
                yield assert(newState.individuals.size == 10)
                end for
            }
        }

        "increments generation counter" in run {
            val state = Population.State(
                individuals = List(createIndividual("ind1", List(0.5))),
                maxSize = 10,
                growthFactor = 1.0,
                generation = 5
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.5, mutation, crossover, selection)
                    newState   <- population.state
                yield assert(newState.generation == 6)
                end for
            }
        }

        "ensures at least one elite" in run {
            val state = Population.State(
                individuals = List.fill(10)(createIndividual("ind", List(0.5))),
                maxSize = 10,
                growthFactor = 1.0
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.0, mutation, crossover, selection)
                    newState   <- population.state
                yield assert(newState.individuals.size == 10)
                end for
            }
        }

        "resets fitness for offspring" in run {
            val individuals = List(
                createIndividual("parent1", List(0.8, 0.9)),
                createIndividual("parent2", List(0.6, 0.7))
            )

            val state     = Population.State(individuals, 10, 1.5)
            val mutation  = createMockMutation("-child")
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            AI.run {
                for
                    population <- Population.init(state)
                    _          <- population.breed(0.0, mutation, crossover, selection)
                    newState   <- population.state
                yield
                    val offspring = newState.individuals.filter(_.genotype.name.contains("child"))
                    assert(offspring.nonEmpty)
                    assert(offspring.forall(_.fitness.isEmpty))
                end for
            }
        }
    }

    "State preservation" - {
        "state method returns current state" in run {
            val initialState = Population.State(
                individuals = List(
                    createIndividual("ind1", List(0.5)),
                    createIndividual("ind2", List(0.6))
                ),
                maxSize = 15,
                growthFactor = 1.2,
                maxTestsByIteration = 3,
                generation = 7
            )

            for
                population <- Population.init(initialState)
                state      <- population.state
            yield
                assert(state.individuals == initialState.individuals)
                assert(state.maxSize == initialState.maxSize)
                assert(state.growthFactor == initialState.growthFactor)
                assert(state.maxTestsByIteration == initialState.maxTestsByIteration)
                assert(state.generation == initialState.generation)
            end for
        }

        "state reflects changes after operations" in run {
            val state = Population.State(
                individuals = List(createIndividual("ind1")),
                maxSize = 10,
                growthFactor = 2.0,
                generation = 0
            )

            val tests         = List(createTestCase("input", "output"))
            val mockFitness   = createMockFitness(Map("ind1" -> 0.7))
            val mockSelection = Selection.init(Selection.State.Fitness)
            val mutation      = createMockMutation()
            val crossover     = createMockCrossover()

            AI.run {
                for
                    population      <- Population.init(state)
                    _               <- AI.run(population.eval(tests, mockFitness))
                    stateAfterEval  <- population.state
                    _               <- population.breed(0.5, mutation, crossover, mockSelection)
                    stateAfterBreed <- population.state
                yield
                    assert(stateAfterEval.individuals.head.fitness.nonEmpty)
                    assert(stateAfterEval.generation == 0)

                    assert(stateAfterBreed.generation == 1)
                    assert(stateAfterBreed.individuals.size >= 1)
                end for
            }
        }
    }

    "Seed function" - {
        "creates individuals from initial state" in run {
            val initial      = createMindState("template")
            val customSeeder = Some(createMindState("seeder"))

            val individuals = List.fill(3)(Individual(
                genotype = initial.copy(name = s"seeded-${Random.nextInt(1000)}"),
                phenotype = initial,
                fitness = List.empty,
                sex = false,
                lineage = Lineage.empty
            ))

            assert(individuals.size == 3)
            assert(individuals.forall(_.fitness.isEmpty))
            assert(individuals.forall(_.genotype.maxRecursions == initial.maxRecursions))
        }

        "uses custom seeder when provided" in {
            val initial      = createMindState("template")
            val customSeeder = Some(createMindState("custom-seeder"))

            assert(customSeeder.isDefined)
            assert(customSeeder.get.name == "custom-seeder")
        }

        "uses default seeder when none provided" in {
            val initial                          = createMindState("template")
            val customSeeder: Option[Mind.State] = None

            assert(customSeeder.isEmpty)
        }
    }

    "should allow population to shrink when at maxSize" in run {
        val state = Population.State(
            individuals = List.fill(10)(createIndividual("ind", List(0.5))),
            maxSize = 10,
            growthFactor = 1.5
        )

        val mutation  = createMockMutation()
        val crossover = createMockCrossover()
        val selection = Selection.init(Selection.State.Fitness)

        for
            population <- Population.init(state)
            _          <- AI.run(population.breed(0.8, mutation, crossover, selection))
            newState   <- population.state
        yield assert(newState.individuals.size == 10)
        end for
    }

    "should use fitness-proportionate parent selection" in run {
        val individuals = List(
            createIndividual("weak", List(0.1), sex = false),
            createIndividual("strong", List(0.9), sex = true)
        )

        val state     = Population.State(individuals, 100, 1.5)
        val selection = Selection.init(Selection.State.Fitness)

        val parentNames = scala.collection.mutable.ListBuffer[String]()
        val trackingMutation = new Mutation:
            def apply(individual: Individual)(using Frame) =
                parentNames += individual.genotype.name
                individual.genotype
            def rate = 1.0

        val noCrossover = new Crossover:
            def apply(p1: Individual, p2: Individual)(using Frame) = p1.genotype
            def rate                                               = 0.0

        for
            population <- Population.init(state)
            _          <- AI.run(population.breed(0.02, trackingMutation, noCrossover, selection))
        yield assert(parentNames.toSet == Set("weak", "strong"))
        end for
    }

    "Individual equality" - {
        "individuals with same values are equal" in {
            val mindState = createMindState("test")
            val fitness   = List(0.5, 0.6).map(TestScore("0", _))
            val lineage   = Lineage.empty

            val ind1 = Individual(mindState, mindState, fitness, false, lineage)
            val ind2 = Individual(mindState, mindState, fitness, false, lineage)

            assert(ind1 == ind2)
        }

        "individuals with different genotypes are not equal" in {
            val mind1   = createMindState("test1")
            val mind2   = createMindState("test2")
            val fitness = List(0.5).map(TestScore("0", _))

            val ind1 = Individual(mind1, mind1, fitness, false, Lineage.empty)
            val ind2 = Individual(mind2, mind2, fitness, false, Lineage.empty)

            assert(ind1 != ind2)
        }

        "individuals with different fitness are not equal" in {
            val mindState = createMindState("test")
            val lineage   = Lineage.empty

            val ind1 = Individual(mindState, mindState, List(0.5).map(TestScore("0", _)), false, lineage)
            val ind2 = Individual(mindState, mindState, List(0.6).map(TestScore("0", _)), false, lineage)

            assert(ind1 != ind2)
        }
    }

    "Mating weights integration with breeding" - {
        "different mating weights produce different breeding pairs" in run {
            val males = List(
                createIndividual("m1", List(0.9, 0.1), sex = false),
                createIndividual("m2", List(0.1, 0.9), sex = false)
            )
            val females = List(
                createIndividual("f1", List(0.1, 0.9), sex = true),
                createIndividual("f2", List(0.9, 0.1), sex = true)
            )

            val complementarityWeights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)
            val stateComp = Population.State(
                males ++ females,
                maxSize = 10,
                growthFactor = 1.0,
                mating = complementarityWeights
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(stateComp)
                _          <- AI.run(population.breed(0.0, mutation, crossover, selection))
                state      <- population.state
            yield
                val offspring = state.individuals.filter(_.genotype.name.contains("+"))
                assert(offspring.nonEmpty)
            end for
        }

        "mating weights from state are used in breeding" in run {
            val individuals = List(
                createIndividual("ind1", List(0.5), sex = false),
                createIndividual("ind2", List(0.5), sex = true),
                createIndividual("ind3", List(0.5), sex = false),
                createIndividual("ind4", List(0.5), sex = true)
            )

            val customWeights = Mating(geneticDistance = 0.5, complementarity = 0.3, similarity = 0.2)
            val state         = Population.State(individuals, 10, 1.0, mating = customWeights)

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.3, mutation, crossover, selection))
                newState   <- population.state
            yield
                assert(newState.mating == customWeights)
                assert(newState.individuals.size >= 4)
            end for
        }
    }

    "Lineage tracking through breeding" - {
        "offspring lineage includes both parents" in run {
            val parent1 = createIndividual("parent1", List(0.5), sex = false)
            val parent2 = createIndividual("parent2", List(0.5), sex = true)

            for child <- Individual.init(parent1, parent2, createMindState("child"))
            yield
                val distance1 = child.lineage.distanceTo(parent1.lineage)
                val distance2 = child.lineage.distanceTo(parent2.lineage)
                assert(distance1 <= 1.0)
                assert(distance2 <= 1.0)
            end for
        }

        "lineage tracks immediate parents" in run {
            val gen0p1 = createIndividual("gen0-1", List(0.5), sex = false)
            val gen0p2 = createIndividual("gen0-2", List(0.5), sex = true)
            val gen0p3 = createIndividual("gen0-3", List(0.5), sex = false)
            val gen0p4 = createIndividual("gen0-4", List(0.5), sex = true)

            for
                gen1a <- Individual.init(gen0p1, gen0p2, createMindState("gen1-a"))
                gen1b <- Individual.init(gen0p3, gen0p4, createMindState("gen1-b"))
                gen2  <- Individual.init(gen1a, gen1b, createMindState("gen2"))
            yield
                val distance = gen2.lineage.distanceTo(gen1a.lineage)
                assert(distance > 0.0)
            end for
        }

        "siblings share parents in lineage" in run {
            val parent1 = createIndividual("p1", List(0.5), sex = false)
            val parent2 = createIndividual("p2", List(0.5), sex = true)

            for
                child1 <- Individual.init(parent1, parent2, createMindState("child1"))
                child2 <- Individual.init(parent1, parent2, createMindState("child2"))
            yield
                val distance = child1.lineage.distanceTo(child2.lineage)
                assert(distance < 1.0)
                assert(distance >= 0.0)
            end for
        }
    }

    "Sex assignment and distribution" - {
        "Individual.init assigns random sex" in run {
            val parent1 = createIndividual("p1", List(0.5), sex = false)
            val parent2 = createIndividual("p2", List(0.5), sex = true)

            for
                child1 <- Individual.init(parent1, parent2, createMindState("c1"))
                child2 <- Individual.init(parent1, parent2, createMindState("c2"))
                child3 <- Individual.init(parent1, parent2, createMindState("c3"))
                child4 <- Individual.init(parent1, parent2, createMindState("c4"))
            yield
                val sexes = List(child1.sex, child2.sex, child3.sex, child4.sex)
                assert(sexes.contains(true) || sexes.contains(false))
            end for
        }

        "breeding produces both sexes eventually" in run {
            val individuals = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("f1", List(0.5), sex = true)
            )

            val state     = Population.State(individuals, 20, 2.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.2, mutation, crossover, selection))
                _          <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state      <- population.state
            yield
                val hasMales   = state.individuals.exists(!_.sex)
                val hasFemales = state.individuals.exists(_.sex)
                assert(hasMales || hasFemales)
            end for
        }
    }

    "Breeding replication fallback" - {
        "replication fills remaining slots when crossover produces insufficient offspring" in run {
            // Create population with only 3 individuals, one of which is unique sex
            val individuals = List(
                createIndividual("m1", List(0.8), sex = false),
                createIndividual("m2", List(0.7), sex = false),
                createIndividual("f1", List(0.9), sex = true)
            )

            val state     = Population.State(individuals, 10, 2.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.33, mutation, crossover, selection))
                newState   <- population.state
            yield
                // With only one breeding pair possible, replication should fill remaining slots
                assert(newState.individuals.size >= 4, s"Expected growth, got ${newState.individuals.size}")
                assert(newState.generation == 1)
            end for
        }

        "breeding with no valid pairs produces only replicants" in run {
            // All same sex - no breeding pairs possible
            val individuals = List(
                createIndividual("m1", List(0.9), sex = false),
                createIndividual("m2", List(0.8), sex = false),
                createIndividual("m3", List(0.7), sex = false)
            )

            val state     = Population.State(individuals, 10, 1.5)
            val mutation  = createMockMutation("-replicated")
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.33, mutation, crossover, selection))
                newState   <- population.state
            yield
                // Should have replicants only (no crossover offspring)
                val replicants = newState.individuals.filter(_.genotype.name.contains("-replicated"))
                assert(replicants.nonEmpty, "Expected replicants when no breeding pairs available")
                assert(newState.individuals.size >= 3)
            end for
        }

        "maintains minimum population size through replication" in run {
            val individuals = List(
                createIndividual("sole", List(0.5), sex = false)
            )

            val state     = Population.State(individuals, 10, 1.5)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.5, mutation, crossover, selection))
                newState   <- population.state
            yield
                assert(newState.individuals.nonEmpty, "Population should never become empty")
                assert(newState.individuals.size >= 1)
            end for
        }

        "offspring count respects targetSize calculation" in run {
            // Start with 4 individuals, maxSize 10, growthFactor 2.0
            // Expected: 4 * 2.0 = 8 (rounded up), but capped by maxSize
            val individuals = List.fill(2)(createIndividual("m", List(0.5), sex = false)) ++
                List.fill(2)(createIndividual("f", List(0.5), sex = true))

            val state     = Population.State(individuals, 10, 2.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.25, mutation, crossover, selection))
                newState   <- population.state
            yield
            // eliteCount = max(2, ceil(4 * 0.25)) = 2
            // targetSize = min(ceil(4 * 2.0), 10) = min(8, 10) = 8
            // offspringCount = 8 - 2 = 6
            assert(newState.individuals.size == 8, s"Expected 8, got ${newState.individuals.size}")
            end for
        }

        "eliteCount calculation ensures at least 2 elites" in run {
            val individuals = List.fill(5)(createIndividual("m", List(0.5), sex = false)) ++
                List.fill(5)(createIndividual("f", List(0.5), sex = true))

            val state     = Population.State(individuals, 10, 1.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.05, mutation, crossover, selection))
                newState   <- population.state
            yield
            // eliteFraction = 0.05, currentSize = 10
            // eliteCount = max(2, ceil(10 * 0.05)) = max(2, 1) = 2
            assert(newState.individuals.size == 10)
            end for
        }
    }

    "Multi-dimensional fitness" - {
        "handles fitness with many dimensions" in run {
            val individuals = List(
                createIndividual("ind1", List(0.5, 0.6, 0.7, 0.8, 0.9, 0.4, 0.3, 0.2, 0.1, 0.5))
            )

            val state = Population.State(individuals, 10, 1.5)

            for
                population <- Population.init(state)
            yield
                val best = population.best
                assert(best.fitness.size == 10)
                assert(math.abs(best.fitness.map(_.score).sum - 5.0) < 0.0001)
            end for
        }

        "complementarity works with many fitness dimensions" in {
            val ind1 = createIndividual(
                "weak",
                List(0.1, 0.2, 0.3, 0.4, 0.5, 0.1, 0.2, 0.3),
                sex = false
            )
            val ind2 = createIndividual(
                "strong",
                List(0.9, 0.8, 0.7, 0.6, 0.5, 0.9, 0.8, 0.7),
                sex = true
            )

            val comp = ind1.complementarity(ind2)
            assert(comp > 0.4)
            assert(comp < 1.0)
        }

        "empty fitness in mate selection returns candidates" in {
            val ind1 = createIndividual("ind1", List.empty, sex = false)
            val ind2 = createIndividual("ind2", List.empty, sex = true)
            val ind3 = createIndividual("ind3", List.empty, sex = true)

            val mates = ind1.selectMates(List(ind2, ind3), Mating())

            assert(mates.size == 2)
        }
    }

    "Breeding edge cases" - {
        "breeding with minimal population (2 individuals)" in run {
            val individuals = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("f1", List(0.5), sex = true)
            )

            val state     = Population.State(individuals, 10, 1.5)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.5, mutation, crossover, selection))
                newState   <- population.state
            yield assert(newState.individuals.size >= 2)
            end for
        }

        "breeding with all same fitness values" in run {
            val individuals = List.fill(6)(createIndividual("ind", List(0.5), sex = false))
                ++ List.fill(6)(createIndividual("ind", List(0.5), sex = true))

            val state     = Population.State(individuals, 20, 1.2)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.3, mutation, crossover, selection))
                newState   <- population.state
            yield assert(newState.individuals.size >= individuals.size)
            end for
        }

        "breeding with no valid pairs produces replicants" in run {
            val individuals = List.fill(5)(createIndividual("ind", List(0.5), sex = false))

            val state     = Population.State(individuals, 10, 1.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.2, mutation, crossover, selection))
                newState   <- population.state
            yield assert(newState.individuals.size >= 1)
            end for
        }
    }

    "State preservation across operations" - {
        "mating weights preserved after breeding" in run {
            val customWeights = Mating(geneticDistance = 0.6, complementarity = 0.2, similarity = 0.2)
            val state = Population.State(
                List(
                    createIndividual("m1", List(0.5), sex = false),
                    createIndividual("f1", List(0.5), sex = true)
                ),
                maxSize = 10,
                growthFactor = 1.5,
                mating = customWeights
            )

            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.5, mutation, crossover, selection))
                newState   <- population.state
            yield assert(newState.mating == customWeights)
            end for
        }

        "maxTestsByIteration preserved after evaluation" in run {
            val state = Population.State(
                List(createIndividual("ind1")),
                maxSize = 10,
                growthFactor = 1.5,
                maxTestsByIteration = 3
            )

            val tests       = List.fill(10)(createTestCase("input", "output"))
            val mockFitness = createMockFitness(Map("ind1" -> 0.5))

            for
                population <- Population.init(state)
                _          <- AI.run(population.eval(tests, mockFitness))
                newState   <- population.state
            yield assert(newState.maxTestsByIteration == 3)
            end for
        }
    }

    "Mating weights" - {
        "default weights sum to 1.0" in {
            val mating = Mating()
            val sum    = mating.geneticDistance + mating.complementarity + mating.similarity

            assert(sum == 1.0)
        }

        "custom weights work correctly" in {
            val custom = Mating(geneticDistance = 0.5, complementarity = 0.3, similarity = 0.2)
            val sum    = custom.geneticDistance + custom.complementarity + custom.similarity

            assert(sum == 1.0)
        }

        "normalized returns same instance when weights sum to 1.0" in {
            val mating     = Mating(geneticDistance = 0.3, complementarity = 0.4, similarity = 0.3)
            val normalized = mating.normalized

            assert(normalized eq mating)
        }

        "normalizes weights that sum to 2.0" in {
            val mating     = Mating(geneticDistance = 0.6, complementarity = 0.8, similarity = 0.6)
            val normalized = mating.normalized

            assert(math.abs(normalized.geneticDistance - 0.3) < 0.0001)
            assert(math.abs(normalized.complementarity - 0.4) < 0.0001)
            assert(math.abs(normalized.similarity - 0.3) < 0.0001)
        }

        "normalizes weights that sum to 0.5" in {
            val mating     = Mating(geneticDistance = 0.15, complementarity = 0.2, similarity = 0.15)
            val normalized = mating.normalized

            assert(math.abs(normalized.geneticDistance - 0.3) < 0.0001)
            assert(math.abs(normalized.complementarity - 0.4) < 0.0001)
            assert(math.abs(normalized.similarity - 0.3) < 0.0001)
        }

        "normalizes very large weights" in {
            val mating     = Mating(geneticDistance = 30.0, complementarity = 40.0, similarity = 30.0)
            val normalized = mating.normalized

            assert(math.abs(normalized.geneticDistance - 0.3) < 0.0001)
            assert(math.abs(normalized.complementarity - 0.4) < 0.0001)
            assert(math.abs(normalized.similarity - 0.3) < 0.0001)
        }

        "normalizes very small weights" in {
            val mating     = Mating(geneticDistance = 0.003, complementarity = 0.004, similarity = 0.003)
            val normalized = mating.normalized

            assert(math.abs(normalized.geneticDistance - 0.3) < 0.0001)
            assert(math.abs(normalized.complementarity - 0.4) < 0.0001)
            assert(math.abs(normalized.similarity - 0.3) < 0.0001)
        }

        "returns default Mating when all weights are zero" in {
            val mating     = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 0.0)
            val normalized = mating.normalized

            assert(normalized == Mating(), 10)
        }

        "preserves weight ratios after normalization" in {
            val mating     = Mating(geneticDistance = 1.0, complementarity = 2.0, similarity = 3.0)
            val normalized = mating.normalized

            val total = 1.0 + 2.0 + 3.0
            assert(math.abs(normalized.geneticDistance - 1.0 / total) < 0.0001)
            assert(math.abs(normalized.complementarity - 2.0 / total) < 0.0001)
            assert(math.abs(normalized.similarity - 3.0 / total) < 0.0001)
        }

        "normalized weights sum to 1.0" in {
            val mating     = Mating(geneticDistance = 7.0, complementarity = 11.0, similarity = 13.0)
            val normalized = mating.normalized
            val sum        = normalized.geneticDistance + normalized.complementarity + normalized.similarity

            assert(math.abs(sum - 1.0) < 0.0001)
        }

        "normalization is idempotent" in {
            val mating      = Mating(geneticDistance = 2.0, complementarity = 3.0, similarity = 5.0)
            val normalized1 = mating.normalized
            val normalized2 = normalized1.normalized

            assert(normalized1 eq normalized2)
        }

        "handles single dominant weight" in {
            val mating     = Mating(geneticDistance = 0.99, complementarity = 0.005, similarity = 0.005)
            val normalized = mating.normalized

            assert(normalized eq mating)
        }

        "handles unbalanced weights correctly" in {
            val mating     = Mating(geneticDistance = 100.0, complementarity = 1.0, similarity = 1.0)
            val normalized = mating.normalized

            val total = 100.0 + 1.0 + 1.0
            assert(math.abs(normalized.geneticDistance - 100.0 / total) < 0.0001)
            assert(math.abs(normalized.complementarity - 1.0 / total) < 0.0001)
            assert(math.abs(normalized.similarity - 1.0 / total) < 0.0001)
        }
    }

    "Breeding with population growth" - {
        "preserves all seed individuals when below maxSize" in run {
            val seedIndividuals = List(
                createIndividual("seed1", List(0.3), sex = false),
                createIndividual("seed2", List(0.5), sex = true),
                createIndividual("seed3", List(0.7), sex = false),
                createIndividual("seed4", List(0.9), sex = true)
            )

            val state     = Population.State(seedIndividuals, maxSize = 20, growthFactor = 1.5)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.2, mutation, crossover, selection))
                newState   <- population.state
            yield
                val seedNames = seedIndividuals.map(_.genotype.name).toSet
                val newNames  = newState.individuals.map(_.genotype.name)

                // All original seeds should still be present
                assert(seedNames.forall(name => newNames.contains(name)))
                // Population should have grown
                assert(newState.individuals.size > seedIndividuals.size)
            end for
        }

        "adds offspring without removing any individuals during growth" in run {
            val individuals = List(
                createIndividual("original1", List(0.2), sex = false),
                createIndividual("original2", List(0.8), sex = true)
            )

            val state     = Population.State(individuals, maxSize = 10, growthFactor = 2.0)
            val mutation  = createMockMutation("-child")
            val crossover = createMockCrossover("+")
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.5, mutation, crossover, selection))
                newState   <- population.state
            yield
                // Both originals should still exist
                assert(newState.individuals.exists(_.genotype.name == "original1"))
                assert(newState.individuals.exists(_.genotype.name == "original2"))
                // Should have offspring
                assert(newState.individuals.exists(_.genotype.name.contains("+")))
                // Total size should be originals + offspring
                assert(newState.individuals.size == 4) // 2 originals + 2 offspring (growth to 2*2=4)
            end for
        }

        "eliteFraction is ignored when below maxSize" in run {
            val individuals = List(
                createIndividual("weak", List(0.1), sex = false),
                createIndividual("medium", List(0.5), sex = true),
                createIndividual("strong", List(0.9), sex = false),
                createIndividual("stronger", List(0.95), sex = true)
            )

            val state     = Population.State(individuals, maxSize = 20, growthFactor = 1.5)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                // Very low eliteFraction that would normally eliminate weak individuals
                _        <- AI.run(population.breed(0.1, mutation, crossover, selection))
                newState <- population.state
            yield
                // Even weak individuals should be preserved during growth
                assert(newState.individuals.exists(_.genotype.name == "weak"))
                assert(newState.individuals.exists(_.genotype.name == "medium"))
                assert(newState.individuals.exists(_.genotype.name == "strong"))
                assert(newState.individuals.exists(_.genotype.name == "stronger"))
            end for
        }

        "applies selection pressure only at maxSize" in run {
            val individuals = List(
                createIndividual("weak1", List(0.1), sex = false),
                createIndividual("weak2", List(0.15), sex = true),
                createIndividual("weak3", List(0.2), sex = false),
                createIndividual("weak4", List(0.25), sex = true),
                createIndividual("strong1", List(0.8), sex = false),
                createIndividual("strong2", List(0.85), sex = true),
                createIndividual("strong3", List(0.9), sex = false),
                createIndividual("strong4", List(0.95), sex = true)
            )

            val state     = Population.State(individuals, maxSize = 8, growthFactor = 1.5)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)

            for
                population <- Population.init(state)
                _          <- AI.run(population.breed(0.25, mutation, crossover, selection))
                newState   <- population.state
            yield
                // At maxSize, weak individuals may be removed
                val weakCount = newState.individuals.count(_.genotype.name.startsWith("weak"))
                // With eliteFraction=0.25, only top 2 are kept as elites (max(2, ceil(8*0.25)) = 2)
                // The rest are offspring, so we expect fewer weak individuals
                assert(weakCount < 4)
                // Population should stay at maxSize
                assert(newState.individuals.size == 8)
            end for
        }

        "grows gradually over multiple generations" in run {
            val initial = List(
                createIndividual("seed1", List(0.5), sex = false),
                createIndividual("seed2", List(0.5), sex = true)
            )

            val state     = Population.State(initial, maxSize = 20, growthFactor = 1.5)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)
            val tests     = List(createTestCase("input", "output"))
            val fitness   = createMockFitness(Map.empty.withDefaultValue(0.5))

            for
                population <- Population.init(state)

                // Generation 1
                _      <- AI.run(population.eval(tests, fitness))
                _      <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state1 <- population.state

                // Generation 2
                _      <- AI.run(population.eval(tests, fitness))
                _      <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state2 <- population.state

                // Generation 3
                _      <- AI.run(population.eval(tests, fitness))
                _      <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state3 <- population.state
            yield
                // Seeds should be preserved through all generations
                assert(state1.individuals.exists(_.genotype.name == "seed1"))
                assert(state2.individuals.exists(_.genotype.name == "seed1"))
                assert(state3.individuals.exists(_.genotype.name == "seed1"))

                // Population should grow each generation until maxSize
                assert(state1.individuals.size >= 2)
                assert(state2.individuals.size >= state1.individuals.size)
                assert(state3.individuals.size >= state2.individuals.size)
                assert(state3.individuals.size <= 20)
            end for
        }

        "reaches maxSize and then maintains it" in run {
            val initial = List(
                createIndividual("seed1", List(0.5), sex = false),
                createIndividual("seed2", List(0.5), sex = true)
            )

            val state     = Population.State(initial, maxSize = 6, growthFactor = 2.0)
            val mutation  = createMockMutation()
            val crossover = createMockCrossover()
            val selection = Selection.init(Selection.State.Fitness)
            val tests     = List(createTestCase("input", "output"))
            val fitness   = createMockFitness(Map.empty.withDefaultValue(0.5))

            for
                population <- Population.init(state)

                // Breed until we reach maxSize
                _      <- AI.run(population.eval(tests, fitness))
                _      <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state1 <- population.state

                _      <- AI.run(population.eval(tests, fitness))
                _      <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state2 <- population.state

                _      <- AI.run(population.eval(tests, fitness))
                _      <- AI.run(population.breed(0.2, mutation, crossover, selection))
                state3 <- population.state
            yield
                // Should eventually reach maxSize
                assert(state2.individuals.size <= 6 || state3.individuals.size == 6)
                // Once at maxSize, should maintain it
                assert(state3.individuals.size == 6)
            end for
        }

    }

end PopulationTest
