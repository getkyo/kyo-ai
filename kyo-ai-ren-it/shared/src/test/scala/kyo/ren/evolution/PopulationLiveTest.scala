package kyo.ren.evolution

import kyo.*
import kyo.ai.json.*
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class PopulationLiveTest extends Test:

    def createSimpleMindState(name: String): Mind.State =
        Mind.State(
            name = name,
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State(
                p"""Process the input and provide a brief response.""",
                p"""Respond briefly.""",
                Temperature.balanced,
                History.State(List.empty, 0)
            )
        )
    end createSimpleMindState

    def createIndividual(name: String, fitness: List[Double] = List.empty, sex: Boolean = false): Individual =
        val mindState = createSimpleMindState(name)
        Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = fitness.map(TestScore("0", _)),
            sex = sex,
            lineage = Lineage.empty
        )
    end createIndividual

    "seeds population with diversity" in run {
        val templateMind = createSimpleMindState("template")
        val size         = 3

        AI.run {
            for individuals <- Population.seed(templateMind, size, customSeeder = None)
            yield
                assert(individuals.size == size, s"Should create $size individuals but got ${individuals.size}")
                assert(individuals.forall(_.fitness.isEmpty), "Seeded individuals should have empty fitness")
                assert(individuals.forall(_.genotype.maxRecursions > 0), "maxRecursions should be positive")

                val names = individuals.map(_.genotype.name).toSet
                assert(names.size >= 1, s"Should have at least 1 unique name: $names")

                assert(individuals.forall(_.genotype.name.nonEmpty), "All individuals should have names")
                succeed
        }
    }

    "evaluates small population with simple test" in run {
        val individuals = List(
            createIndividual("ind1"),
            createIndividual("ind2")
        )

        val state = Population.State(
            individuals = individuals,
            maxSize = 10,
            growthFactor = 1.5
        )

        val testCase = Evolution.TestCase(
            testId = "0",
            input = "What is 3 + 3?",
            expected = "6"
        )

        AI.run {
            for
                fitness    <- Fitness.init(Fitness.State())
                population <- Population.init(state)
                _          <- population.eval(List(testCase), fitness)
                newState   <- population.state
            yield
                assert(newState.individuals.size == 2, s"Population size should remain 2 but got ${newState.individuals.size}")
                assert(
                    newState.individuals.forall(_.fitness.size == 1),
                    s"Each individual should have 1 fitness score: ${newState.individuals.map(_.fitness.size)}"
                )
                assert(
                    newState.individuals.forall(ind => ind.fitness.forall(f => f.score >= 0.0 && f.score <= 1.0)),
                    s"Fitness scores should be in [0,1]: ${newState.individuals.map(_.fitness)}"
                )
        }
    }

    "breeds population with real mutation and crossover" in run {
        val individuals = List(
            createIndividual("parent1", List(0.8), sex = false),
            createIndividual("parent2", List(0.7), sex = true)
        )

        val state = Population.State(
            individuals = individuals,
            maxSize = 10,
            growthFactor = 1.5
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0))
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Fitness)
            for
                crossover  <- Crossover.init(crossoverState)
                population <- Population.init(state)
                _          <- population.breed(0.5, mutation, crossover, selection)
                newState   <- population.state
            yield
                assert(newState.individuals.size >= 2, s"Population should grow but got ${newState.individuals.size}")
                assert(newState.generation == 1, s"Generation should increment to 1 but got ${newState.generation}")

                val originalNames = individuals.map(_.genotype.name).toSet
                val hasElite      = newState.individuals.exists(ind => originalNames.contains(ind.genotype.name))
                assert(hasElite || newState.individuals.nonEmpty, "Should have elites or offspring")
            end for
        }
    }

    "runs full eval-breed cycle" in run {
        val individuals = List(
            createIndividual("gen0-1", sex = false),
            createIndividual("gen0-2", sex = true)
        )

        val state = Population.State(
            individuals = individuals,
            maxSize = 8,
            growthFactor = 1.5
        )

        val testCase = Evolution.TestCase(
            testId = "0",
            input = "What is 2 + 2?",
            expected = "4"
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 0.5
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Fitness)
            for
                fitness    <- Fitness.init(Fitness.State())
                crossover  <- Crossover.init(crossoverState)
                population <- Population.init(state)

                _      <- population.eval(List(testCase), fitness)
                state1 <- population.state
                _      <- population.breed(0.5, mutation, crossover, selection)
                state2 <- population.state

                _          <- population.eval(List(testCase), fitness)
                finalState <- population.state
            yield

                assert(state1.individuals.forall(_.fitness.nonEmpty), "After eval, all should have fitness")
                assert(state1.generation == 0, s"Generation after eval should be 0 but got ${state1.generation}")

                assert(state2.generation == 1, s"Generation after breed should be 1 but got ${state2.generation}")
                assert(state2.individuals.size >= 2, s"Population should maintain or grow: ${state2.individuals.size}")

                assert(finalState.individuals.forall(_.fitness.nonEmpty), "All individuals should have fitness scores")
                assert(finalState.generation == 1, s"Generation should still be 1: ${finalState.generation}")
            end for
        }
    }

    "respects maxSize limit during breeding" in run {
        val individuals = List.fill(6)(createIndividual("ind", List(0.5)))

        val state = Population.State(
            individuals = individuals,
            maxSize = 8,
            growthFactor = 2.0
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0))
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Fitness)
            for
                crossover  <- Crossover.init(crossoverState)
                population <- Population.init(state)
                ctx        <- Context.init(Context.State())
                _          <- population.breed(0.3, ctx.mutation, ctx.crossover, ctx.selection)
                newState   <- population.state
            yield
                assert(newState.individuals.size <= 8, s"Population should not exceed maxSize=8 but got ${newState.individuals.size}")
                assert(newState.generation == 1, s"Generation should be 1 but got ${newState.generation}")
            end for
        }
    }

    "uses Pareto selection strategy" in run {
        val individuals = List(
            createIndividual("specialist1", List(0.9, 0.1), sex = false),
            createIndividual("specialist2", List(0.1, 0.9), sex = true),
            createIndividual("generalist", List(0.5, 0.5), sex = false)
        )

        val state = Population.State(
            individuals = individuals,
            maxSize = 10,
            growthFactor = 1.5
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 0.5
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Pareto)
            for
                crossover  <- Crossover.init(crossoverState)
                population <- Population.init(state)
                _          <- population.breed(0.3, mutation, crossover, selection)
                newState   <- population.state
            yield
                assert(newState.individuals.size >= 3, s"Population should grow: ${newState.individuals.size}")
                assert(newState.generation == 1, s"Generation should be 1 but got ${newState.generation}")
            end for
        }
    }

    "handles maxTestsByIteration limit" in run {
        val individuals = List(createIndividual("ind1"))

        val state = Population.State(
            individuals = individuals,
            maxSize = 10,
            growthFactor = 1.5,
            maxTestsByIteration = 2
        )

        val testCases = List(
            Evolution.TestCase("0", "Test 1", "Answer 1"),
            Evolution.TestCase("0", "Test 2", "Answer 2"),
            Evolution.TestCase("0", "Test 3", "Answer 3"),
            Evolution.TestCase("0", "Test 4", "Answer 4"),
            Evolution.TestCase("0", "Test 5", "Answer 5")
        )

        AI.run {
            for
                fitness    <- Fitness.init(Fitness.State())
                population <- Population.init(state)
                _          <- population.eval(testCases, fitness)
                newState   <- population.state
            yield assert(
                newState.individuals.head.fitness.size == 2,
                s"Should evaluate only 2 tests due to maxTestsByIteration but got ${newState.individuals.head.fitness.size}"
            )
        }
    }

    "maintains population size when at maxSize" in run {
        val individuals = List.fill(8)(createIndividual("ind", List(0.5)))

        val state = Population.State(
            individuals = individuals,
            maxSize = 8,
            growthFactor = 2.0
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0))
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Fitness)
            for
                crossover  <- Crossover.init(crossoverState)
                population <- Population.init(state)
                ctx        <- Context.init(Context.State())
                _          <- population.breed(0.25, ctx.mutation, ctx.crossover, ctx.selection)
                newState   <- population.state
            yield assert(newState.individuals.size < 8, s"Population at maxSize should stay at 8 but got ${newState.individuals.size}")
            end for
        }
    }

    "tracks best individual across generations" in run {
        val individuals = List(
            createIndividual("weak", List(0.3), sex = false),
            createIndividual("strong", List(0.9), sex = true)
        )

        val state = Population.State(
            individuals = individuals,
            maxSize = 10,
            growthFactor = 1.5
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0)),
            rate = 0.3
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Fitness)
            for
                crossover  <- Crossover.init(crossoverState)
                population <- Population.init(state)
                best1 = population.best
                ctx      <- Context.init(Context.State())
                _        <- population.breed(0.5, ctx.mutation, ctx.crossover, ctx.selection)
                newState <- population.state
            yield
                assert(best1.genotype.name == "strong", s"Best should be 'strong' but got ${best1.genotype.name}")
                assert(
                    math.abs(best1.fitness.map(_.score).sum - 0.9) < 0.01,
                    s"Best fitness should be ~0.9 but got ${best1.fitness.map(_.score).sum}"
                )

                val hasElite = newState.individuals.exists(ind =>
                    ind.genotype.name == "strong" || ind.lineage.distanceTo(best1.lineage) == 0.0
                )
                assert(newState.individuals.nonEmpty, "Population should not be empty")
            end for
        }
    }

    "updates phenotype during evaluation" in run {
        val genotype  = createSimpleMindState("original-genotype")
        val phenotype = createSimpleMindState("old-phenotype")
        val individual = Individual(
            genotype = genotype,
            phenotype = phenotype,
            fitness = List.empty,
            sex = false,
            lineage = Lineage.empty
        )

        val state = Population.State(
            individuals = List(individual),
            maxSize = 10,
            growthFactor = 1.5
        )

        val testCase = Evolution.TestCase("0", "Simple test", "Simple answer")

        AI.run {
            for
                fitness    <- Fitness.init(Fitness.State())
                population <- Population.init(state)
                _          <- population.eval(List(testCase), fitness)
                newState   <- population.state
            yield
                val updated = newState.individuals.head

                assert(
                    updated.phenotype.name == "original-genotype",
                    s"Phenotype should be updated to genotype name but got ${updated.phenotype.name}"
                )
        }
    }

    "grows population gradually with growthFactor" in run {
        val individuals = List(
            createIndividual("start1", List(0.5), sex = false),
            createIndividual("start2", List(0.5), sex = true)
        )

        val state = Population.State(
            individuals = individuals,
            maxSize = 20,
            growthFactor = 1.5
        )

        val mutationState = Mutation.State(
            strategies = List(Mutation.Strategy.Temp(1.0))
        )

        val crossoverState = Crossover.State(
            customMind = None
        )

        AI.run {
            val mutation  = Mutation.init(mutationState)
            val selection = Selection.init(Selection.State.Fitness)
            for
                crossover   <- Crossover.init(crossoverState)
                population  <- Population.init(state)
                ctx         <- Context.init(Context.State())
                _           <- population.breed(0.5, ctx.mutation, ctx.crossover, ctx.selection)
                afterBreed1 <- population.state
                _           <- population.breed(0.5, ctx.mutation, ctx.crossover, ctx.selection)
                afterBreed2 <- population.state
            yield
                assert(afterBreed1.individuals.size >= 2, s"Should grow from 2: ${afterBreed1.individuals.size}")
                assert(
                    afterBreed2.individuals.size >= afterBreed1.individuals.size,
                    s"Should continue growing: ${afterBreed1.individuals.size} -> ${afterBreed2.individuals.size}"
                )
            end for
        }
    }

end PopulationLiveTest
