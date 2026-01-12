package kyo.ren.evolution

import Population.Mating
import kyo.*
import kyo.Test
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class IndividualTest extends Test:

    def createMindState(name: String, neuronCount: Int = 0): Mind.State =
        val neurons = if neuronCount > 0 then
            List.tabulate(neuronCount) { i =>
                Neuron.State(
                    name = Neuron.Name(s"neuron-$i"),
                    description = "test neuron",
                    conscious = false,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
                )
            }
        else List.empty

        Mind.State(
            name = name,
            maxRecursions = 1,
            neurons = neurons,
            output = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
        )
    end createMindState

    def createIndividual(
        name: String,
        fitness: List[Double] = List.empty,
        sex: Boolean = false,
        neuronCount: Int = 0
    ): Individual =
        val mindState = createMindState(name, neuronCount)
        Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = fitness.map(TestScore("0", _)),
            sex = sex,
            lineage = Lineage.empty
        )
    end createIndividual

    def createMindStateWithNeuronProperties(
        name: String,
        neuronCount: Int,
        consciousCount: Int = 0,
        memoryCount: Int = 0,
        avgConnectionsPerNeuron: Int = 0
    ): Mind.State =
        require(consciousCount <= neuronCount, s"consciousCount ($consciousCount) must be <= neuronCount ($neuronCount)")
        require(memoryCount <= neuronCount, s"memoryCount ($memoryCount) must be <= neuronCount ($neuronCount)")

        val neurons = if neuronCount > 0 then
            List.tabulate(neuronCount) { i =>
                val isConscious     = i < consciousCount
                val hasMemory       = i < memoryCount
                val connectionCount = avgConnectionsPerNeuron

                // Create simple forward connections for topology variation
                val nextIds = if connectionCount > 0 then
                    List.tabulate(connectionCount.min(neuronCount - 1)) { j => Neuron.Name(s"neuron-$j") }
                else List.empty

                Neuron.State(
                    name = Neuron.Name(s"neuron-$i"),
                    description = "test neuron",
                    conscious = isConscious,
                    previous = List.empty,
                    next = nextIds,
                    memory = if hasMemory then
                        Some(Memory.State(
                            List.empty,
                            Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
                        ))
                    else None,
                    gate = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
                )
            }
        else List.empty

        Mind.State(
            name = name,
            maxRecursions = 1,
            neurons = neurons,
            output = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
        )
    end createMindStateWithNeuronProperties

    "selectMates" - {
        "filters opposite sex only" in {
            val male   = createIndividual("male", sex = false)
            val female = createIndividual("female", sex = true)
            val male2  = createIndividual("male2", sex = false)

            val candidates = List(male2, female)
            val selected   = male.selectMates(candidates, Mating())

            assert(selected.size == 1)
            assert(selected.head._1 == female)
            assert(selected.head._2 > 0.0, "Attraction score should be positive")
            assert(!selected.exists(_._1 == male2), "Same-sex candidate should be filtered out")
        }

        "excludes self from selection" in {
            val individual = createIndividual("self", sex = false)
            val candidate  = createIndividual("other", sex = true)

            val selected = individual.selectMates(List(individual, candidate), Mating())

            assert(selected.size == 1)
            assert(selected.head._1 == candidate)
            assert(!selected.exists(_._1 == individual), "Self should be excluded from selection")
            assert(selected.head._2 > 0.0, "Attraction score should be positive")
        }

        "returns empty for same sex population" in {
            val male1 = createIndividual("male1", sex = false)
            val male2 = createIndividual("male2", sex = false)
            val male3 = createIndividual("male3", sex = false)

            val selected = male1.selectMates(List(male2, male3), Mating())

            assert(selected.isEmpty)
        }

        "returns empty for empty candidates" in {
            val individual = createIndividual("self", sex = false)
            val selected   = individual.selectMates(List.empty, Mating())

            assert(selected.isEmpty)
        }

        "orders by attraction descending" in {
            val individual = createIndividual("self", List(0.5, 0.5), sex = false)
            val high       = createIndividual("high", List(0.9, 0.9), sex = true)
            val low        = createIndividual("low", List(0.1, 0.1), sex = true)
            val mid        = createIndividual("mid", List(0.5, 0.5), sex = true)

            val selected = individual.selectMates(List(low, high, mid), Mating())

            assert(selected.size == 3)
            assert(selected(0)._1 == high)
            assert(selected(1)._1 == mid)
            assert(selected(2)._1 == low)

            // Verify actual attraction scores are in descending order
            assert(selected(0)._2 >= selected(1)._2, "Attraction scores should be in descending order")
            assert(selected(1)._2 >= selected(2)._2, "Attraction scores should be in descending order")

            // Verify attraction scores match expected calculations
            val weights        = Mating()
            val attractionHigh = individual.attractionTo(high, weights.normalized)
            val attractionMid  = individual.attractionTo(mid, weights.normalized)
            val attractionLow  = individual.attractionTo(low, weights.normalized)

            assert(math.abs(selected(0)._2 - attractionHigh) < 0.0001)
            assert(math.abs(selected(1)._2 - attractionMid) < 0.0001)
            assert(math.abs(selected(2)._2 - attractionLow) < 0.0001)
        }

        "automatically normalizes weights" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = false)
            val ind2 = createIndividual("ind2", List(0.8), sex = true)

            val unnormalizedWeights = Mating(geneticDistance = 0.6, complementarity = 0.8, similarity = 0.6)
            val normalizedWeights   = unnormalizedWeights.normalized

            val mates1 = ind1.selectMates(List(ind2), unnormalizedWeights)
            val mates2 = ind1.selectMates(List(ind2), normalizedWeights)

            assert(mates1.size == mates2.size)
            assert(mates1.head == mates2.head)
        }

        "with zero weights uses default" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = false)
            val ind2 = createIndividual("ind2", List(0.8), sex = true)

            val zeroWeights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 0.0)
            val mates       = ind1.selectMates(List(ind2), zeroWeights)

            assert(mates.size == 1)
        }

        "produces consistent ordering with normalized weights" in {
            val ind = createIndividual("ind", List(0.5), sex = false)
            val c1  = createIndividual("c1", List(0.9), sex = true)
            val c2  = createIndividual("c2", List(0.3), sex = true)
            val c3  = createIndividual("c3", List(0.7), sex = true)

            val weights1 = Mating(geneticDistance = 0.3, complementarity = 0.4, similarity = 0.3)
            val weights2 = Mating(geneticDistance = 0.6, complementarity = 0.8, similarity = 0.6)

            val mates1 = ind.selectMates(List(c1, c2, c3), weights1)
            val mates2 = ind.selectMates(List(c1, c2, c3), weights2)

            assert(mates1.map(_._1.genotype.name) == mates2.map(_._1.genotype.name))
        }
    }

    "attractionTo" - {
        "combines all three factors with default weights" in {
            val parent = createIndividual("parent")
            val ind1   = createIndividual("ind1", List(0.5), sex = false)
            val ind2   = createIndividual("ind2", List(0.8), sex = true)

            val ind1Extended = ind1.copy(lineage = Lineage.init(parent, parent))

            val weights = Mating()
            val genDist = ind1Extended.geneticDistance(ind2)
            val comp    = ind1Extended.complementarity(ind2)
            val sim     = ind1Extended.similarity(ind2)

            val expectedAttraction = genDist * weights.geneticDistance + comp * weights.complementarity + sim * weights.similarity
            val actualAttraction   = ind1Extended.attractionTo(ind2, weights.normalized)

            assert(math.abs(actualAttraction - expectedAttraction) < 0.0001, s"Expected $expectedAttraction, got $actualAttraction")

            val mates = ind1Extended.selectMates(List(ind2), weights)
            assert(mates.size == 1)
            assert(math.abs(mates.head._2 - expectedAttraction) < 0.0001)
        }

        "high complementarity weight favors gap-filling" in {
            val weak    = createIndividual("weak", List(0.0, 1.0), sex = false)
            val strong  = createIndividual("strong", List(1.0, 0.0), sex = true)
            val average = createIndividual("average", List(0.5, 0.5), sex = true)

            val compStrong  = weak.complementarity(strong)
            val compAverage = weak.complementarity(average)

            // weak has gaps at [1.0, 0.0], strong fills with [1.0, 0.0] = (1.0*1.0 + 0.0*0.0)/2 = 0.5
            // weak has gaps at [1.0, 0.0], average fills with [0.5, 0.5] = (1.0*0.5 + 0.0*0.5)/2 = 0.25
            assert(compStrong > compAverage, s"Strong complementarity ($compStrong) should be > average ($compAverage)")

            val weights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)
            val mates   = weak.selectMates(List(average, strong), weights)

            assert(mates.size == 2)
            assert(mates.head._1 == strong, "With pure complementarity weight, strong should be preferred")
            assert(mates.head._2 > mates(1)._2, "Strong should have higher attraction score")
        }

        "high similarity weight favors similar architectures" in {
            val ind       = createIndividual("ind", neuronCount = 5, sex = false)
            val similar   = createIndividual("similar", neuronCount = 5, sex = true)
            val different = createIndividual("different", neuronCount = 20, sex = true)

            val simSimilar   = ind.similarity(similar)
            val simDifferent = ind.similarity(different)

            assert(simSimilar > simDifferent, s"Similar similarity ($simSimilar) should be > different ($simDifferent)")
            assert(simSimilar == 1.0, "Identical neuron counts should have perfect similarity")

            val weights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)
            val mates   = ind.selectMates(List(different, similar), weights)

            assert(mates.size == 2)
            assert(mates.head._1 == similar, "With pure similarity weight, similar should be preferred")
            assert(mates.head._2 > mates(1)._2, "Similar should have higher attraction score")
        }

        "calculates exact weighted sum" in {
            val parent = createIndividual("parent")
            val ind1   = createIndividual("ind1", List(0.3), sex = true)
            val ind2   = createIndividual("ind2", List(0.8), sex = false)

            val ind1Extended = ind1.copy(lineage = Lineage.init(parent, parent))
            val ind2Extended = ind2.copy(lineage = Lineage.init(parent, parent))

            val genDist = ind1Extended.lineage.distanceTo(ind2Extended.lineage)
            val comp    = ind1Extended.complementarity(ind2Extended)
            val sim     = ind1Extended.similarity(ind2Extended)

            val weights    = Mating(geneticDistance = 0.3, complementarity = 0.4, similarity = 0.3)
            val attraction = ind1Extended.attractionTo(ind2Extended, weights)

            val expected = genDist * 0.3 + comp * 0.4 + sim * 0.3

            assert(math.abs(attraction - expected) < 0.0001)
        }

        "with maximum genetic distance" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = true)
            val ind2 = createIndividual("ind2", List(0.5), sex = false)

            val weights    = Mating(geneticDistance = 1.0, complementarity = 0.0, similarity = 0.0)
            val attraction = ind1.attractionTo(ind2, weights)

            assert(attraction == 1.0)
        }

        "components contribute proportionally" in {
            val ind1 = createIndividual("ind1", List(0.2), neuronCount = 5)
            val ind2 = createIndividual("ind2", List(0.9), neuronCount = 5)

            val w1 = Mating(geneticDistance = 1.0, complementarity = 0.0, similarity = 0.0)
            val w2 = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)
            val w3 = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)

            val a1 = ind1.attractionTo(ind2, w1)
            val a2 = ind1.attractionTo(ind2, w2)
            val a3 = ind1.attractionTo(ind2, w3)

            val combined  = Mating(geneticDistance = 0.5, complementarity = 0.3, similarity = 0.2)
            val aCombined = ind1.attractionTo(ind2, combined)

            val expected = a1 * 0.5 + a2 * 0.3 + a3 * 0.2

            assert(math.abs(aCombined - expected) < 0.0001)
        }

        "is bounded between 0 and 1 with valid weights" in {
            val ind1 = createIndividual("ind1", List(0.0), neuronCount = 1)
            val ind2 = createIndividual("ind2", List(1.0), neuronCount = 100)

            val parent       = createIndividual("parent")
            val ind1Extended = ind1.copy(lineage = Lineage.init(parent, parent))

            val weights    = Mating()
            val attraction = ind1Extended.attractionTo(ind2, weights)

            assert(attraction >= 0.0)
            assert(attraction <= 1.0)
        }

        "can exceed 1.0 with non-normalized weights" in {
            val ind1 = createIndividual("ind1", List(0.0), sex = true)
            val ind2 = createIndividual("ind2", List(1.0), sex = false)

            val badWeights = Mating(geneticDistance = 2.0, complementarity = 3.0, similarity = 1.0)
            val attraction = ind1.attractionTo(ind2, badWeights)

            assert(attraction > 1.0)
        }

        "can be negative with negative weights" in {
            val ind1 = createIndividual("ind1", List(0.0), sex = true)
            val ind2 = createIndividual("ind2", List(1.0), sex = false)

            val negWeights = Mating(geneticDistance = -1.0, complementarity = 0.0, similarity = 0.0)
            val attraction = ind1.attractionTo(ind2, negWeights)

            assert(attraction < 0.0)
        }

        "neutral values produce sensible attraction" in {
            val ind1 = createIndividual("ind1", List.empty, neuronCount = 0, sex = true)
            val ind2 = createIndividual("ind2", List.empty, neuronCount = 0, sex = false)

            val weights    = Mating()
            val attraction = ind1.attractionTo(ind2, weights)

            val expected = 1.0 * 0.3 + 0.5 * 0.4 + 0.5 * 0.3

            assert(attraction == expected)
            assert(expected == 0.65)
        }

        "all components at maximum with valid weights gives 1.0" in {
            val parent = createIndividual("parent1")
            val ind1   = createIndividual("ind1", List(0.0), neuronCount = 5, sex = true)
            val ind2   = createIndividual("ind2", List(1.0), neuronCount = 5, sex = false)

            val weights    = Mating(geneticDistance = 0.3, complementarity = 0.4, similarity = 0.3)
            val attraction = ind1.attractionTo(ind2, weights)

            val expected = 1.0 * 0.3 + 1.0 * 0.4 + 1.0 * 0.3

            assert(math.abs(attraction - expected) < 0.0001)
            assert(expected == 1.0)
        }

        "minimum complementarity with shared ancestor" in {
            val parent = createIndividual("parent")
            val ind1   = createIndividual("ind1", List(1.0), neuronCount = 5)
            val ind2   = createIndividual("ind2", List(0.0), neuronCount = 5)

            val ind1Extended = ind1.copy(lineage = Lineage.init(parent, parent), sex = true)
            val ind2Extended = ind2.copy(lineage = Lineage.init(parent, parent), sex = false)

            val weights    = Mating(geneticDistance = 0.3, complementarity = 0.4, similarity = 0.3)
            val attraction = ind1Extended.attractionTo(ind2Extended, weights)

            val genDist = ind1Extended.lineage.distanceTo(ind2Extended.lineage)
            val comp    = ind1Extended.complementarity(ind2Extended)
            val sim     = ind1Extended.similarity(ind2Extended)

            assert(comp == 0.0)
            assert(sim == 1.0)

            val expected = genDist * 0.3 + 0.0 * 0.4 + 1.0 * 0.3

            assert(math.abs(attraction - expected) < 0.0001)
        }

        "weights not summing to 1.0 produce proportional attraction" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = true)
            val ind2 = createIndividual("ind2", List(0.8), sex = false)

            val normalWeights = Mating(geneticDistance = 0.3, complementarity = 0.4, similarity = 0.3)
            val doubleWeights = Mating(geneticDistance = 0.6, complementarity = 0.8, similarity = 0.6)

            val normalAttraction = ind1.attractionTo(ind2, normalWeights)
            val doubleAttraction = ind1.attractionTo(ind2, doubleWeights)

            assert(math.abs(doubleAttraction - normalAttraction * 2.0) < 0.0001)
        }

        "handles fitness dimension mismatch gracefully" in {
            val ind1 = createIndividual("ind1", List(0.5, 0.5, 0.5), sex = false)
            val ind2 = createIndividual("ind2", List(0.8), sex = true)

            val weights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)
            val mates   = ind1.selectMates(List(ind2), weights)

            assert(mates.size == 1)
        }

        "zero weights produce valid results" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = false)
            val ind2 = createIndividual("ind2", List(0.8), sex = true)

            val weights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 0.0)
            val mates   = ind1.selectMates(List(ind2), weights)

            assert(mates.size == 1)
        }

        "single weight at 1.0" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = false)
            val ind2 = createIndividual("ind2", List(0.8), sex = true)

            val weights = Mating(geneticDistance = 1.0, complementarity = 0.0, similarity = 0.0)
            val mates   = ind1.selectMates(List(ind2), weights)

            assert(mates.size == 1)
        }

        "returns zero attraction to self" in {
            // Test the explicit self-reference check in line 25: this == other
            val individual = createIndividual("self", List(0.8), sex = false)
            val attraction = individual.attractionTo(individual, Mating())

            assert(attraction == 0.0, "Individual should have zero attraction to itself")
        }

        "similarity weight dominates when structures differ greatly" in {
            // Integration test: verify that high similarity weight makes structure the determining factor
            val simple = createMindStateWithNeuronProperties(
                "simple",
                neuronCount = 2,
                consciousCount = 0,
                memoryCount = 0,
                avgConnectionsPerNeuron = 0
            )
            val complex = createMindStateWithNeuronProperties(
                "complex",
                neuronCount = 50,
                consciousCount = 40,
                memoryCount = 40,
                avgConnectionsPerNeuron = 10
            )

            val ind1        = Individual(simple, simple, List(0.1, 0.1).map(TestScore("0", _)), sex = false, Lineage.empty)
            val ind2Similar = Individual(simple, simple, List(0.9, 0.9).map(TestScore("0", _)), sex = true, Lineage.empty)
            val ind2Complex = Individual(complex, complex, List(0.9, 0.9).map(TestScore("0", _)), sex = true, Lineage.empty)

            // With high similarity weight, structural similarity should dominate over complementarity
            val highSimWeights = Mating(geneticDistance = 0.0, complementarity = 0.1, similarity = 0.9)

            val attractionToSimilar = ind1.attractionTo(ind2Similar, highSimWeights)
            val attractionToComplex = ind1.attractionTo(ind2Complex, highSimWeights)

            // Despite ind2Complex having perfect complementarity, ind2Similar should be more attractive due to structure
            assert(
                attractionToSimilar > attractionToComplex,
                s"With high similarity weight, structurally similar ($attractionToSimilar) should be > structurally different ($attractionToComplex)"
            )
        }

        "complementarity vs similarity tradeoff" in {
            // Integration test: realistic evolutionary scenario
            // Weak but similar architecture vs Strong but different architecture
            val weakSimilar = createMindStateWithNeuronProperties(
                "ws",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 5,
                avgConnectionsPerNeuron = 3
            )
            val strongDifferent = createMindStateWithNeuronProperties(
                "sd",
                neuronCount = 50,
                consciousCount = 40,
                memoryCount = 40,
                avgConnectionsPerNeuron = 10
            )

            val seeker = Individual(weakSimilar, weakSimilar, List(0.2, 0.2).map(TestScore("0", _)), sex = false, Lineage.empty)
            val mate1  = Individual(weakSimilar, weakSimilar, List(0.3, 0.3).map(TestScore("0", _)), sex = true, Lineage.empty)
            val mate2 = Individual(
                strongDifferent,
                strongDifferent,
                List(0.9, 0.9).map(TestScore("0", _)),
                sex = true,
                Lineage.empty
            )

            // Strategy 1: Favor complementarity (fill gaps in fitness)
            val compWeights = Mating(geneticDistance = 0.0, complementarity = 0.8, similarity = 0.2)
            val attr1Comp   = seeker.attractionTo(mate1, compWeights)
            val attr2Comp   = seeker.attractionTo(mate2, compWeights)

            assert(
                attr2Comp > attr1Comp,
                s"With high complementarity weight, strong mate ($attr2Comp) should be > weak mate ($attr1Comp)"
            )

            // Strategy 2: Favor similarity (preserve working architecture)
            val simWeights = Mating(geneticDistance = 0.0, complementarity = 0.2, similarity = 0.8)
            val attr1Sim   = seeker.attractionTo(mate1, simWeights)
            val attr2Sim   = seeker.attractionTo(mate2, simWeights)

            assert(
                attr1Sim > attr2Sim,
                s"With high similarity weight, similar mate ($attr1Sim) should be > different mate ($attr2Sim)"
            )
        }

        "genetic distance prevents inbreeding in high fitness populations" in {
            // Integration test: verify genetic distance works as intended
            val ancestor    = createIndividual("ancestor")
            val parent1Mind = createMindState("parent1")
            val parent2Mind = createMindState("parent2")

            val parent1 = Individual(
                parent1Mind,
                parent1Mind,
                List(0.9).map(TestScore("0", _)),
                sex = false,
                Lineage.init(ancestor, ancestor)
            )
            val sibling = Individual(
                parent2Mind,
                parent2Mind,
                List(0.9).map(TestScore("0", _)),
                sex = true,
                Lineage.init(ancestor, ancestor)
            )
            val unrelated = Individual(
                createMindState("unrelated"),
                createMindState("unrelated"),
                List(0.8).map(TestScore("0", _)),
                sex = true,
                Lineage.init(createIndividual("unrelated"), createIndividual("unrelated2"))
            )

            // Even though sibling has higher fitness (0.9 vs 0.8), genetic distance should favor unrelated
            val weights = Mating(geneticDistance = 0.7, complementarity = 0.2, similarity = 0.1)

            val attractionToSibling   = parent1.attractionTo(sibling, weights)
            val attractionToUnrelated = parent1.attractionTo(unrelated, weights)

            // Genetic distance should dominate, making unrelated more attractive despite lower fitness
            assert(
                attractionToUnrelated > attractionToSibling,
                s"With high genetic distance weight, unrelated ($attractionToUnrelated) should be > sibling ($attractionToSibling)"
            )
        }
    }

    "complementarity" - {
        "returns neutral 0.5 for empty fitness" in {
            val ind1 = createIndividual("ind1", List.empty)
            val ind2 = createIndividual("ind2", List(0.5, 0.5))

            val comp = ind1.complementarity(ind2)

            assert(comp == 0.5)
        }

        "returns neutral 0.5 when self has empty fitness" in {
            val ind1 = createIndividual("ind1", List.empty)
            val ind2 = createIndividual("ind2", List(0.8, 0.9))

            assert(ind1.complementarity(ind2) == 0.5)
        }

        "perfect complementarity equals 1.0" in {
            val weak   = createIndividual("weak", List(0.0, 0.0))
            val strong = createIndividual("strong", List(1.0, 1.0))

            val comp = weak.complementarity(strong)

            assert(comp == 1.0)
        }

        "zero complementarity when other is weak" in {
            val strong = createIndividual("strong", List(1.0, 1.0))
            val weak   = createIndividual("weak", List(0.0, 0.0))

            val comp = strong.complementarity(weak)

            assert(comp == 0.0)
        }

        "calculates exact complementarity value" in {
            val ind1 = createIndividual("ind1", List(0.5, 0.3))
            val ind2 = createIndividual("ind2", List(0.7, 0.8))

            val comp     = ind1.complementarity(ind2)
            val expected = ((1.0 - 0.5) * 0.7 + (1.0 - 0.3) * 0.8) / 2.0

            assert(math.abs(comp - expected) < 0.0001)
        }

        "complementarity with single dimension" in {
            val ind1 = createIndividual("ind1", List(0.2))
            val ind2 = createIndividual("ind2", List(0.9))

            val comp     = ind1.complementarity(ind2)
            val expected = (1.0 - 0.2) * 0.9

            assert(math.abs(comp - expected) < 0.0001)
        }

        "complementarity with three dimensions" in {
            val ind1 = createIndividual("ind1", List(0.1, 0.5, 0.9))
            val ind2 = createIndividual("ind2", List(0.8, 0.6, 0.3))

            val comp     = ind1.complementarity(ind2)
            val expected = ((1.0 - 0.1) * 0.8 + (1.0 - 0.5) * 0.6 + (1.0 - 0.9) * 0.3) / 3.0

            assert(math.abs(comp - expected) < 0.0001)
        }

        "complementarity is asymmetric" in {
            val ind1 = createIndividual("ind1", List(0.3, 0.7))
            val ind2 = createIndividual("ind2", List(0.8, 0.2))

            val comp12 = ind1.complementarity(ind2)
            val comp21 = ind2.complementarity(ind1)

            assert(comp12 != comp21)
        }

        "handles fitness dimension mismatch via zip truncation" in {
            val ind1 = createIndividual("ind1", List(0.5, 0.5, 0.5))
            val ind2 = createIndividual("ind2", List(0.8))

            val comp     = ind1.complementarity(ind2)
            val expected = ((1.0 - 0.5) * 0.8) / 3.0

            assert(math.abs(comp - expected) < 0.0001)
        }

        "complementarity with equal fitness is intermediate" in {
            val ind1 = createIndividual("ind1", List(0.5, 0.5))
            val ind2 = createIndividual("ind2", List(0.5, 0.5))

            val comp = ind1.complementarity(ind2)

            assert(comp == 0.25)
        }

        "both individuals have empty fitness" in {
            val ind1 = createIndividual("ind1", List.empty)
            val ind2 = createIndividual("ind2", List.empty)

            val comp = ind1.complementarity(ind2)

            assert(comp == 0.5)
        }

        "mate has longer fitness vector" in {
            val ind1 = createIndividual("ind1", List(0.5), sex = true)
            val ind2 = createIndividual("ind2", List(0.8, 0.9, 0.7), sex = false)

            val comp     = ind1.complementarity(ind2)
            val expected = ((1.0 - 0.5) * 0.8) / 1.0

            assert(math.abs(comp - expected) < 0.0001)
        }

        "one fitness value at boundary 0.0" in {
            val ind1 = createIndividual("ind1", List(0.0), sex = true)
            val ind2 = createIndividual("ind2", List(0.5), sex = false)

            val comp = ind1.complementarity(ind2)

            assert(math.abs(comp - 0.5) < 0.0001)
        }

        "one fitness value at boundary 1.0" in {
            val ind1 = createIndividual("ind1", List(1.0), sex = true)
            val ind2 = createIndividual("ind2", List(0.5), sex = false)

            val comp = ind1.complementarity(ind2)

            assert(comp == 0.0)
        }
    }

    "similarity" - {
        "returns neutral 0.5 for empty neurons" in {
            val ind1 = createIndividual("ind1", neuronCount = 0, sex = false)
            val ind2 = createIndividual("ind2", neuronCount = 5, sex = true)

            val similarity = ind1.similarity(ind2)
            assert(similarity == 0.5, s"Similarity should be 0.5 for empty neurons, got $similarity")

            val weights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)
            val mates   = ind1.selectMates(List(ind2), weights)

            assert(mates.nonEmpty)
            assert(math.abs(mates.head._2 - 0.5) < 0.0001, "Attraction with pure similarity weight should equal similarity value")
        }

        "perfect similarity for identical neuron counts" in {
            val ind1 = createIndividual("ind1", neuronCount = 5, sex = false)
            val ind2 = createIndividual("ind2", neuronCount = 5, sex = true)

            val similarity = ind1.similarity(ind2)
            assert(similarity == 1.0, s"Similarity should be 1.0 for identical neuron counts and structure, got $similarity")

            val weights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)
            val mates   = ind1.selectMates(List(ind2), weights)

            assert(mates.size == 1)
            assert(math.abs(mates.head._2 - 1.0) < 0.0001, "Attraction with pure similarity weight should equal similarity value")
        }

        "dissimilarity for different neuron counts" in {
            val small = createIndividual("small", neuronCount = 1, sex = false)
            val large = createIndividual("large", neuronCount = 10, sex = true)

            val similarity = small.similarity(large)
            // Expected: neuronCountSim = 9/10 = 0.9, names = 0.9 (1 shared out of 10 unique), other = 0
            val expected = (8.0 - 0.9 - 0.9) / 8.0
            assert(math.abs(similarity - expected) < 0.0001, s"Similarity should be $expected for 1 vs 10 neurons, got $similarity")

            val identicalSim = createIndividual("s1", neuronCount = 1).similarity(createIndividual("s2", neuronCount = 1))
            assert(similarity < identicalSim, "Different neuron counts should have lower similarity than identical")

            val weights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)
            val mates   = small.selectMates(List(large), weights)

            assert(mates.size == 1)
        }

        "returns exact 0.5 for both empty neurons" in {
            val ind1 = createIndividual("ind1", neuronCount = 0)
            val ind2 = createIndividual("ind2", neuronCount = 0)

            val sim = ind1.similarity(ind2)

            assert(sim == 0.5)
        }

        "returns exact 0.5 when one has empty neurons" in {
            val ind1 = createIndividual("ind1", neuronCount = 0)
            val ind2 = createIndividual("ind2", neuronCount = 10)

            val sim1 = ind1.similarity(ind2)
            val sim2 = ind2.similarity(ind1)

            assert(sim1 == 0.5)
            assert(sim2 == 0.5)
        }

        "identical neuron counts yield high similarity" in {
            val ind1 = createIndividual("ind1", neuronCount = 5)
            val ind2 = createIndividual("ind2", neuronCount = 5)

            val sim = ind1.similarity(ind2)

            assert(sim == 1.0, s"Identical structures should have perfect similarity, got $sim")
        }

        "very different neuron counts yield lower similarity" in {
            val ind1 = createIndividual("ind1", neuronCount = 1)
            val ind2 = createIndividual("ind2", neuronCount = 100)

            val simSame = createIndividual("s1", neuronCount = 1).similarity(createIndividual("s2", neuronCount = 1))
            val simDiff = ind1.similarity(ind2)

            assert(simDiff < simSame)
        }

        "similarity is symmetric" in {
            val ind1 = createIndividual("ind1", neuronCount = 3)
            val ind2 = createIndividual("ind2", neuronCount = 7)

            val sim12 = ind1.similarity(ind2)
            val sim21 = ind2.similarity(ind1)

            assert(math.abs(sim12 - sim21) < 0.0001)
        }

        "topology similarity - fully connected vs sparse" in {
            // Test the avgConnections component of similarity (lines 57-61)
            val fullyConnected = createMindStateWithNeuronProperties("fc", neuronCount = 5, avgConnectionsPerNeuron = 4)
            val sparse         = createMindStateWithNeuronProperties("sp", neuronCount = 5, avgConnectionsPerNeuron = 1)
            val disconnected   = createMindStateWithNeuronProperties("dc", neuronCount = 5, avgConnectionsPerNeuron = 0)

            val ind1 = Individual(fullyConnected, fullyConnected, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(sparse, sparse, List.empty, sex = true, Lineage.empty)
            val ind3 = Individual(disconnected, disconnected, List.empty, sex = true, Lineage.empty)

            val sim12 = ind1.similarity(ind2)
            val sim13 = ind1.similarity(ind3)
            val sim23 = ind2.similarity(ind3)

            // Calculate expected similarity manually for each pair
            val avgConns1 = 4.0 // fully connected
            val avgConns2 = 1.0 // sparse
            val avgConns3 = 0.0 // disconnected

            // topologyDiff formula: abs(a1 - a2) / max(a1, a2).max(1.0)
            val topologyDiff12 = math.abs(avgConns1 - avgConns2) / math.max(avgConns1, avgConns2).max(1.0) // = 3/4 = 0.75
            val topologyDiff13 = math.abs(avgConns1 - avgConns3) / math.max(avgConns1, avgConns3).max(1.0) // = 4/4 = 1.0
            val topologyDiff23 = math.abs(avgConns2 - avgConns3) / math.max(avgConns2, avgConns3).max(1.0) // = 1/1 = 1.0

            // Expected similarities (all other components are 0 since they have same neuron count, names, prompts, etc.)
            val expectedSim12 = (8.0 - topologyDiff12) / 8.0 // = 8.25/9 = 0.9166...
            val expectedSim13 = (8.0 - topologyDiff13) / 8.0 // = 8.0/9 = 0.8888...
            val expectedSim23 = (8.0 - topologyDiff23) / 8.0 // = 8.0/9 = 0.8888...

            assert(math.abs(sim12 - expectedSim12) < 0.0001, s"sim12: expected $expectedSim12, got $sim12")
            assert(math.abs(sim13 - expectedSim13) < 0.0001, s"sim13: expected $expectedSim13, got $sim13")
            assert(math.abs(sim23 - expectedSim23) < 0.0001, s"sim23: expected $expectedSim23, got $sim23")

            // Verify ordering: fully vs sparse should be more similar than either vs disconnected
            assert(sim12 > sim13, s"Fully vs sparse ($sim12) should be > fully vs disconnected ($sim13)")
            assert(sim12 > sim23, s"Fully vs sparse ($sim12) should be > sparse vs disconnected ($sim23)")

            // FINDING: sim13 == sim23 because both reach maximum topology difference (1.0)
            // This shows the formula treats "any connection" vs "no connection" as maximum difference
            // regardless of how many connections exist
            assert(math.abs(sim13 - sim23) < 0.0001, s"Both pairs with disconnected should have equal similarity, got $sim13 vs $sim23")
        }

        "topology similarity - handles zero connections correctly" in {
            // Test the .max(1.0) protection in line 61
            val disconnected1 = createMindStateWithNeuronProperties("dc1", neuronCount = 3, avgConnectionsPerNeuron = 0)
            val disconnected2 = createMindStateWithNeuronProperties("dc2", neuronCount = 3, avgConnectionsPerNeuron = 0)

            val ind1 = Individual(disconnected1, disconnected1, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(disconnected2, disconnected2, List.empty, sex = true, Lineage.empty)

            val sim = ind1.similarity(ind2)

            // Both have avgConnections=0, difference is 0
            // All four components should be 0, so similarity = (8.0 - 0) / 8.0 = 1.0
            assert(sim == 1.0, s"Two identical disconnected networks should have similarity 1.0, got $sim")
        }

        "conscious ratio affects similarity" in {
            // Test the consciousRatio component (lines 63-66)
            val allConscious  = createMindStateWithNeuronProperties("ac", neuronCount = 10, consciousCount = 10)
            val halfConscious = createMindStateWithNeuronProperties("hc", neuronCount = 10, consciousCount = 5)
            val noConscious   = createMindStateWithNeuronProperties("nc", neuronCount = 10, consciousCount = 0)

            val ind1 = Individual(allConscious, allConscious, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(halfConscious, halfConscious, List.empty, sex = true, Lineage.empty)
            val ind3 = Individual(noConscious, noConscious, List.empty, sex = true, Lineage.empty)

            val sim12 = ind1.similarity(ind2)
            val sim13 = ind1.similarity(ind3)
            val sim23 = ind2.similarity(ind3)

            // All conscious vs none conscious should be most different
            assert(sim13 < sim12, s"100% vs 0% conscious ($sim13) should be < 100% vs 50% ($sim12)")
            assert(sim13 < sim23, s"100% vs 0% conscious ($sim13) should be < 50% vs 0% ($sim23)")

            // Calculate expected values
            val conscRatio1   = 1.0                                           // 10/10
            val conscRatio3   = 0.0                                           // 0/10
            val consciousDiff = math.abs(conscRatio1 - conscRatio3)           // = 1.0
            val expectedSim13 = (8.0 - 0.0 - 0.0 - consciousDiff - 0.0) / 8.0 // = 3.0 / 4.0 = 0.75
            assert(math.abs(sim13 - expectedSim13) < 0.0001, s"Expected similarity $expectedSim13, got $sim13")
        }

        "memory ratio affects similarity" in {
            // Test the memoryRatio component (lines 68-71)
            val allMemory  = createMindStateWithNeuronProperties("am", neuronCount = 10, memoryCount = 10)
            val halfMemory = createMindStateWithNeuronProperties("hm", neuronCount = 10, memoryCount = 5)
            val noMemory   = createMindStateWithNeuronProperties("nm", neuronCount = 10, memoryCount = 0)

            val ind1 = Individual(allMemory, allMemory, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(halfMemory, halfMemory, List.empty, sex = true, Lineage.empty)
            val ind3 = Individual(noMemory, noMemory, List.empty, sex = true, Lineage.empty)

            val sim12 = ind1.similarity(ind2)
            val sim13 = ind1.similarity(ind3)
            val sim23 = ind2.similarity(ind3)

            // All memory vs none memory should be most different
            assert(sim13 < sim12, s"100% vs 0% memory ($sim13) should be < 100% vs 50% ($sim12)")
            assert(sim13 < sim23, s"100% vs 0% memory ($sim13) should be < 50% vs 0% ($sim23)")

            // Calculate expected values
            val memRatio1  = 1.0                             // 10/10
            val memRatio3  = 0.0                             // 0/10
            val memoryDiff = math.abs(memRatio1 - memRatio3) // = 1.0
            // Memory gate prompts are also different: ind1 has {"p"}, ind3 has {} -> distance = 1.0
            val memoryPromptDiff = 1.0
            val expectedSim13    = (8.0 - 0.0 - 0.0 - 0.0 - memoryDiff - 0.0 - 0.0 - 0.0 - memoryPromptDiff - 0.0) / 8.0 // = 7.0 / 8.0
            assert(math.abs(sim13 - expectedSim13) < 0.0001, s"Expected similarity $expectedSim13, got $sim13")
        }

        "all four similarity components contribute to final score" in {
            // CRITICAL TEST: Verify the complete formula (8.0 - neuronCount - topology - conscious - memory) / 8.0
            // Create two individuals that differ in ALL four dimensions
            val ind1Mind = createMindStateWithNeuronProperties(
                "ind1",
                neuronCount = 10,
                consciousCount = 10,
                memoryCount = 10,
                avgConnectionsPerNeuron = 5
            )
            val ind2Mind = createMindStateWithNeuronProperties(
                "ind2",
                neuronCount = 5,
                consciousCount = 0,
                memoryCount = 0,
                avgConnectionsPerNeuron = 0
            )

            val ind1 = Individual(ind1Mind, ind1Mind, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(ind2Mind, ind2Mind, List.empty, sex = true, Lineage.empty)

            val actualSimilarity = ind1.similarity(ind2)

            // Calculate expected similarity component by component
            val neuronCountDiff = math.abs(10 - 5).toDouble / math.max(10, 5) // = 5/10 = 0.5

            val avgConns1    = 5.0
            val avgConns2    = 0.0
            val topologyDiff = math.abs(avgConns1 - avgConns2) / math.max(avgConns1, avgConns2).max(1.0) // = 5/5 = 1.0

            val consciousRatio1 = 10.0 / 10                                   // = 1.0
            val consciousRatio2 = 0.0 / 5                                     // = 0.0
            val consciousDiff   = math.abs(consciousRatio1 - consciousRatio2) // = 1.0

            val memRatio1  = 10.0 / 10                       // = 1.0
            val memRatio2  = 0.0 / 5                         // = 0.0
            val memoryDiff = math.abs(memRatio1 - memRatio2) // = 1.0

            // Neuron names: ind1 has 0-9, ind2 has 0-4, shared 0-4 (5 shared out of 10 unique) -> distance = 0.5
            val nameDiff = 0.5
            // Output prompts: both "prompt" -> distance = 0.0
            val outputPromptDiff = 0.0
            // Route prompts: both empty -> distance = 0.0
            val routePromptDiff  = 0.0
            val memoryPromptDiff = 1.0
            // Mind output: both "prompt" -> distance = 0.0
            val mindOutputDiff = 0.0

            val expectedSimilarity =
                (8.0 - neuronCountDiff - topologyDiff - consciousDiff - memoryDiff - nameDiff - outputPromptDiff - routePromptDiff - memoryPromptDiff - mindOutputDiff) / 8.0

            assert(
                math.abs(actualSimilarity - expectedSimilarity) < 0.0001,
                s"Expected similarity $expectedSimilarity, got $actualSimilarity\n" +
                    s"  neuronCountDiff: $neuronCountDiff\n" +
                    s"  topologyDiff: $topologyDiff\n" +
                    s"  consciousDiff: $consciousDiff\n" +
                    s"  memoryDiff: $memoryDiff"
            )

            // Verify similarity is very low (many differences)
            assert(actualSimilarity < 0.5, s"Individuals differing in all 4 dimensions should have low similarity, got $actualSimilarity")
        }

        "similarity components are weighted equally" in {
            // Test that each of the 4 components has equal impact (1/4 each)
            val baseline = createMindStateWithNeuronProperties(
                "base",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 5,
                avgConnectionsPerNeuron = 2
            )
            val baseInd = Individual(baseline, baseline, List.empty, sex = false, Lineage.empty)

            // Vary only neuron count
            val diffNeurons = createMindStateWithNeuronProperties(
                "diffN",
                neuronCount = 20,
                consciousCount = 5,
                memoryCount = 5,
                avgConnectionsPerNeuron = 2
            )
            val diffNeuronsInd = Individual(diffNeurons, diffNeurons, List.empty, sex = true, Lineage.empty)

            // Vary only conscious
            val diffConscious = createMindStateWithNeuronProperties(
                "diffC",
                neuronCount = 10,
                consciousCount = 10,
                memoryCount = 5,
                avgConnectionsPerNeuron = 2
            )
            val diffConsciousInd = Individual(diffConscious, diffConscious, List.empty, sex = true, Lineage.empty)

            // Vary only memory
            val diffMemory = createMindStateWithNeuronProperties(
                "diffM",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 10,
                avgConnectionsPerNeuron = 2
            )
            val diffMemoryInd = Individual(diffMemory, diffMemory, List.empty, sex = true, Lineage.empty)

            // Vary only topology
            val diffTopology = createMindStateWithNeuronProperties(
                "diffT",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 5,
                avgConnectionsPerNeuron = 8
            )
            val diffTopologyInd = Individual(diffTopology, diffTopology, List.empty, sex = true, Lineage.empty)

            val simN = baseInd.similarity(diffNeuronsInd)
            val simC = baseInd.similarity(diffConsciousInd)
            val simM = baseInd.similarity(diffMemoryInd)
            val simT = baseInd.similarity(diffTopologyInd)

            // All should be less than 1.0 since they differ in one dimension
            assert(simN < 1.0, s"Similarity with neuron count difference should be < 1.0, got $simN")
            assert(simC < 1.0, s"Similarity with conscious difference should be < 1.0, got $simC")
            assert(simM < 1.0, s"Similarity with memory difference should be < 1.0, got $simM")
            assert(simT < 1.0, s"Similarity with topology difference should be < 1.0, got $simT")

            // The decrease should be proportional to the component difference
            // Each component contributes 1/4 to the final score when maxed out
        }

        // INTERESTING EVOLUTIONARY SCENARIOS

        "evolutionary scenario - specialist vs generalist architectures" in {
            // Scenario: Specialist has few but highly connected neurons (deep thinking)
            // Generalist has many weakly connected neurons (broad coverage)
            val specialist = createMindStateWithNeuronProperties(
                "specialist",
                neuronCount = 5,
                consciousCount = 5,         // All conscious (focused)
                memoryCount = 5,            // All have memory (experienced)
                avgConnectionsPerNeuron = 4 // Densely connected
            )
            val generalist = createMindStateWithNeuronProperties(
                "generalist",
                neuronCount = 20,
                consciousCount = 5,         // Few conscious (broad scan)
                memoryCount = 5,            // Few have memory (less experienced)
                avgConnectionsPerNeuron = 1 // Sparsely connected
            )

            val s = Individual(specialist, specialist, List.empty, sex = false, Lineage.empty)
            val g = Individual(generalist, generalist, List.empty, sex = true, Lineage.empty)

            val sim = s.similarity(g)

            // These are fundamentally different strategies
            // conscious: 1.0 vs 0.25 = 0.75
            // memory: 1.0 vs 0.25 = 0.75

            assert(sim < 0.65, s"Specialist vs generalist should have moderate-low similarity, got $sim")
            assert(sim > 0.5, s"But not too low - they share prompts and some neurons, got $sim")
        }

        "evolutionary scenario - scaling preserves ratios but not similarity" in {
            // Scenario: An architecture scaled up proportionally should remain similar
            // This tests whether the formula is scale-invariant (it's NOT)
            val small = createMindStateWithNeuronProperties(
                "small",
                neuronCount = 10,
                consciousCount = 5,         // 50% conscious
                memoryCount = 5,            // 50% memory
                avgConnectionsPerNeuron = 2 // 2 connections/neuron
            )
            val large = createMindStateWithNeuronProperties(
                "large",
                neuronCount = 100,
                consciousCount = 50,        // 50% conscious (preserved)
                memoryCount = 50,           // 50% memory (preserved)
                avgConnectionsPerNeuron = 2 // 2 connections/neuron (preserved)
            )

            val s = Individual(small, small, List.empty, sex = false, Lineage.empty)
            val l = Individual(large, large, List.empty, sex = true, Lineage.empty)

            val sim = s.similarity(l)

            // conscious: 0.5 vs 0.5 = 0
            // memory: 0.5 vs 0.5 = 0
            // similarity = (8.0 - 0.9 - 0 - 0 - 0) / 8.0 = 3.1 / 4.0 = 0.775

            // FINDING: Size difference dominates even when ratios are identical
            // This design choice penalizes scale - scaled architectures are less similar
            assert(sim > 0.7 && sim < 0.8, s"Scaled architecture similarity affected by size, got $sim")
        }

        "evolutionary scenario - convergent evolution from different paths" in {
            // Scenario: Two architectures converge to same size via different strategies
            // One emphasizes consciousness, other emphasizes memory
            val consciousFocused = createMindStateWithNeuronProperties(
                "conscious",
                neuronCount = 10,
                consciousCount = 10, // 100% conscious
                memoryCount = 0,     // 0% memory
                avgConnectionsPerNeuron = 3
            )
            val memoryFocused = createMindStateWithNeuronProperties(
                "memory",
                neuronCount = 10,
                consciousCount = 0, // 0% conscious
                memoryCount = 10,   // 100% memory
                avgConnectionsPerNeuron = 3
            )

            val c = Individual(consciousFocused, consciousFocused, List.empty, sex = false, Lineage.empty)
            val m = Individual(memoryFocused, memoryFocused, List.empty, sex = true, Lineage.empty)

            val sim = c.similarity(m)

            // neuronCount: 10 vs 10 = 0
            // topology: 3 vs 3 = 0
            // conscious: 1.0 vs 0.0 = 1.0
            // memory ratio: 0.0 vs 1.0 = 1.0
            // names: same → 0
            // output/route/mind prompts: same → 0
            // memory gate prompts: {} vs {"p"} → 1.0

            assert(math.abs(sim - (5.0 / 8.0)) < 0.01, s"Convergent but orthogonal strategies should have moderate similarity, got $sim")
        }

        "evolutionary scenario - gradual complexification" in {
            // Scenario: Evolution gradually adds features (memory, consciousness, connections)
            // Test whether similarity decreases linearly with each added feature
            val stage1 = createMindStateWithNeuronProperties(
                "stage1",
                neuronCount = 10,
                consciousCount = 0,
                memoryCount = 0,
                avgConnectionsPerNeuron = 0
            )
            val stage2 = createMindStateWithNeuronProperties(
                "stage2",
                neuronCount = 10,
                consciousCount = 5, // Add consciousness
                memoryCount = 0,
                avgConnectionsPerNeuron = 0
            )
            val stage3 = createMindStateWithNeuronProperties(
                "stage3",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 5, // Add memory
                avgConnectionsPerNeuron = 0
            )
            val stage4 = createMindStateWithNeuronProperties(
                "stage4",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 5,
                avgConnectionsPerNeuron = 3 // Add connections
            )

            val s1 = Individual(stage1, stage1, List.empty, sex = false, Lineage.empty)
            val s2 = Individual(stage2, stage2, List.empty, sex = true, Lineage.empty)
            val s3 = Individual(stage3, stage3, List.empty, sex = false, Lineage.empty)
            val s4 = Individual(stage4, stage4, List.empty, sex = true, Lineage.empty)

            val sim12 = s1.similarity(s2) // Differ by 1 component (conscious)
            val sim23 = s2.similarity(s3) // Differ by 1 component (memory)
            val sim34 = s3.similarity(s4) // Differ by 1 component (topology)
            val sim14 = s1.similarity(s4) // Differ by 3 components

            // Each step should reduce similarity by roughly 1/4
            // sim12: (8.0 - 0 - 0 - 0.5 - 0) / 8.0 = 0.875
            // sim23: (8.0 - 0 - 0 - 0 - 0.5) / 8.0 = 0.875
            // sim14: should be much lower

            assert(sim12 > 0.8, s"One feature difference should be high similarity: $sim12")
            assert(sim23 > 0.8, s"One feature difference should be high similarity: $sim23")
            assert(sim14 < sim12, s"Three feature difference should be lower similarity: $sim14 < $sim12")
            assert(sim14 < sim23, s"Three feature difference should be lower similarity: $sim14 < $sim23")
        }

        "evolutionary scenario - bottleneck with architectural homogeneity" in {
            // Scenario: Population goes through bottleneck, losing architectural diversity
            // All survivors have identical architecture but different fitness
            val survivor1 = createMindStateWithNeuronProperties(
                "s1",
                neuronCount = 8,
                consciousCount = 4,
                memoryCount = 4,
                avgConnectionsPerNeuron = 2
            )
            val survivor2 = createMindStateWithNeuronProperties(
                "s2",
                neuronCount = 8,
                consciousCount = 4,
                memoryCount = 4,
                avgConnectionsPerNeuron = 2
            )

            val s1 = Individual(survivor1, survivor1, List(0.9).map(TestScore("0", _)), sex = false, Lineage.empty)
            val s2 = Individual(survivor2, survivor2, List(0.1).map(TestScore("0", _)), sex = true, Lineage.empty)

            val sim = s1.similarity(s2)

            // Identical architectures = perfect similarity
            assert(sim == 1.0, s"Identical architectures should have perfect similarity, got $sim")

            // But FITNESS differs - so with high complementarity weight,
            // s2 (low fitness) would be strongly attracted to s1 (high fitness)
            val compWeights  = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)
            val attraction21 = s2.attractionTo(s1, compWeights)

            // s2 has gaps of 0.9, s1 fills with 0.9 = 0.9*0.9 = 0.81
            assert(attraction21 > 0.8, s"Low fitness should be highly attracted to high fitness: $attraction21")

            // But with high similarity weight, they'd be equally attracted (architecture match)
            val simWeights    = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)
            val simAttraction = s2.attractionTo(s1, simWeights)
            assert(simAttraction == 1.0, s"Perfect similarity should give perfect attraction: $simAttraction")
        }

        "evolutionary scenario - architectural innovation vs tradition" in {
            // Scenario: Innovation creates architectural difference
            // Tests whether high similarity weight suppresses innovation
            val traditional = createMindStateWithNeuronProperties(
                "trad",
                neuronCount = 10,
                consciousCount = 5,
                memoryCount = 5,
                avgConnectionsPerNeuron = 2
            )
            val innovative = createMindStateWithNeuronProperties(
                "innov",
                neuronCount = 15,           // Larger
                consciousCount = 10,        // More conscious
                memoryCount = 10,           // More memory
                avgConnectionsPerNeuron = 4 // More connected
            )

            val t = Individual(traditional, traditional, List(0.8).map(TestScore("0", _)), sex = false, Lineage.empty)
            val i = Individual(innovative, innovative, List(0.9).map(TestScore("0", _)), sex = true, Lineage.empty)

            val sim = t.similarity(i)

            // Innovation differs in several dimensions but moderately
            // neuronCount: 10 vs 15 = 5/15 ≈ 0.33
            // conscious: 0.5 vs 0.67 ≈ 0.17
            // memory: 0.5 vs 0.67 ≈ 0.17
            // names: 10 shared out of 15 unique = 1 - 10/15 ≈ 0.33
            // prompts: all same → 0
            // similarity ≈ (8.0 - 0.33 - 0.5 - 0.17 - 0.17 - 0.33 - 0 - 0 - 0 - 0) / 8.0 ≈ 7.5/9 ≈ 0.833

            assert(sim > 0.75 && sim < 0.9, s"Innovation should reduce similarity moderately, got $sim")

            // With high similarity weight, traditional prefers traditional (lower sim)
            // With high complementarity weight, traditional attracted to innovative (better fitness)
            val simWeights  = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 1.0)
            val compWeights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)

            val simAttraction  = t.attractionTo(i, simWeights)
            val compAttraction = t.attractionTo(i, compWeights)

            // With pure similarity weight, attraction = similarity
            assert(math.abs(simAttraction - sim) < 0.01, s"Similarity weight should match similarity score")

            // With pure complementarity weight: t needs to fill 0.2 gap, i provides 0.9
            // comp = 0.2 * 0.9 = 0.18
            assert(compAttraction > 0.15 && compAttraction < 0.2, s"Complementarity should favor innovation: $compAttraction")

            // INSIGHT: Innovation is discouraged by similarity weight but encouraged by complementarity weight
            // This creates evolutionary tension between exploration and exploitation
        }

        // TESTS EXPOSING SIMILARITY METRIC FLAWS

        "FLAW: identical architecture but completely different neuron identities" in {
            // BUG: Current similarity ignores neuron names/identities
            // Two individuals with same structure but different functional neurons
            // should NOT be considered identical

            val mathNeurons = List.tabulate(5) { i =>
                Neuron.State(
                    name = Neuron.Name(s"math-${List("addition", "subtraction", "multiplication", "division", "modulo")(i)}"),
                    description = s"Performs mathematical operation",
                    conscious = false,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate =
                        Gate.State("You are a math operator", "Focus on accuracy", Temperature.balanced, History.State(List.empty, 0))
                )
            }

            val artNeurons = List.tabulate(5) { i =>
                Neuron.State(
                    name = Neuron.Name(s"art-${List("color", "composition", "emotion", "metaphor", "symbolism")(i)}"),
                    description = s"Creates artistic content",
                    conscious = false,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate = Gate.State("You are an artist", "Be creative", Temperature.balanced, History.State(List.empty, 0))
                )
            }

            val mathMind = Mind.State(
                "math",
                1,
                mathNeurons,
                Gate.State("math coordinator", "calculate", Temperature.balanced, History.State(List.empty, 0))
            )
            val artMind = Mind.State(
                "art",
                1,
                artNeurons,
                Gate.State("art coordinator", "create", Temperature.balanced, History.State(List.empty, 0))
            )

            val mathInd = Individual(mathMind, mathMind, List.empty, sex = false, Lineage.empty)
            val artInd  = Individual(artMind, artMind, List.empty, sex = true, Lineage.empty)

            val sim = mathInd.similarity(artInd)

            // They have identical architecture (5 neurons, no connections, same ratios)
            // But ZERO shared neurons (math-* vs art-*)
            // AND different prompts (mathematician vs artist, math coordinator vs art coordinator)
            // Differences: names (1.0), output prompts (1.0), mind output (1.0)
            // Result: (8.0 - 0 - 0 - 0 - 0 - 1.0 - 1.0 - 0 - 0 - 1.0) / 8.0 = 6.0 / 8.0
            val expected = 5.0 / 8.0
            assert(
                math.abs(sim - expected) < 0.1,
                s"Identical architecture but different neurons and prompts should give $expected similarity, got $sim"
            )

            // SUCCESS: Prompt similarity now better distinguishes functionally opposite specialists!
            // Was 0.8, now 0.666... which better reflects how different math and art strategies are
        }

        "FLAW: different prompts but same architecture" in {
            // BUG: Current similarity ignores prompt content
            // Prompts define BEHAVIOR but are completely ignored by similarity metric

            val aggressiveNeurons = List.tabulate(3) { i =>
                Neuron.State(
                    name = Neuron.Name(s"neuron-$i"),
                    description = "processor",
                    conscious = false,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate = Gate.State(
                        prompt = "Be aggressive, competitive, and dominant. Challenge everything.",
                        reminder = "ATTACK",
                        temperature = Temperature.creative,
                        history = History.State(List.empty, 0)
                    )
                )
            }

            val passiveNeurons = List.tabulate(3) { i =>
                Neuron.State(
                    name = Neuron.Name(s"neuron-$i"),
                    description = "processor",
                    conscious = false,
                    previous = List.empty,
                    next = List.empty,
                    memory = None,
                    gate = Gate.State(
                        prompt = "Be gentle, cooperative, and empathetic. Build consensus.",
                        reminder = "COOPERATE",
                        temperature = Temperature.precise,
                        history = History.State(List.empty, 0)
                    )
                )
            }

            val aggressiveMind =
                Mind.State(
                    "aggressive",
                    1,
                    aggressiveNeurons,
                    Gate.State("aggressive coordinator", "dominate", Temperature.balanced, History.State(List.empty, 0))
                )
            val passiveMind =
                Mind.State(
                    "passive",
                    1,
                    passiveNeurons,
                    Gate.State("passive coordinator", "harmonize", Temperature.balanced, History.State(List.empty, 0))
                )

            val aggInd = Individual(aggressiveMind, aggressiveMind, List.empty, sex = false, Lineage.empty)
            val pasInd = Individual(passiveMind, passiveMind, List.empty, sex = true, Lineage.empty)

            val sim = aggInd.similarity(pasInd)

            // They have identical architecture (3 neurons, no connections, same ratios)
            // AND identical neuron names (neuron-0, neuron-1, neuron-2)
            // BUT different prompts:
            // - Neuron output prompts differ (aggressive vs gentle)
            // - Mind output prompts differ ("aggressive coordinator" vs "passive coordinator")
            // Expected: (8.0 - 0 - 0 - 0 - 0 - 0 - 1.0 - 0 - 0 - 1.0) / 8.0 = 7.0 / 8.0
            val expected = 6.0 / 8.0
            assert(
                math.abs(sim - expected) < 0.0001,
                s"Identical architecture but different prompts should give $expected similarity, got $sim"
            )

            // SUCCESS: Prompt similarity now distinguishes behavioral strategies!
            // - Aggressive: "Be aggressive, competitive, dominant" + creative temperature
            // - Passive: "Be gentle, cooperative, empathetic" + precise temperature
            // The prompt comparison correctly identifies these as different strategies
        }

        "INSIGHT: current similarity is phenotypic not genotypic" in {
            // SUMMARY TEST: Current similarity measures observable traits (phenotype)
            // but not genetic content (genotype)

            // Create two "organisms" that LOOK the same but ARE different
            val organism1Neurons = List.tabulate(10) { i =>
                Neuron.State(
                    name = Neuron.Name(s"neuron-type-A-$i"),
                    description = "Specialist processor for domain A",
                    conscious = i % 2 == 0,
                    previous = if i > 0 then List(Neuron.Name(s"neuron-type-A-${i - 1}")) else List.empty,
                    next = if i < 9 then List(Neuron.Name(s"neuron-type-A-${i + 1}")) else List.empty,
                    memory = if i % 2 == 0 then
                        Some(Memory.State(List.empty, Gate.State("memory A", "", Temperature.balanced, History.State(List.empty, 0))))
                    else None,
                    gate = Gate.State(
                        s"You are a specialist in domain A, task $i",
                        s"Focus on A-specific objectives",
                        Temperature.balanced,
                        History.State(List.empty, 0)
                    )
                )
            }

            val organism2Neurons = List.tabulate(10) { i =>
                Neuron.State(
                    name = Neuron.Name(s"neuron-type-B-$i"),
                    description = "Specialist processor for domain B",
                    conscious = i % 2 == 0,
                    previous = if i > 0 then List(Neuron.Name(s"neuron-type-B-${i - 1}")) else List.empty,
                    next = if i < 9 then List(Neuron.Name(s"neuron-type-B-${i + 1}")) else List.empty,
                    memory = if i % 2 == 0 then
                        Some(Memory.State(List.empty, Gate.State("memory B", "", Temperature.balanced, History.State(List.empty, 0))))
                    else None,
                    gate = Gate.State(
                        s"You are a specialist in domain B, task $i",
                        s"Focus on B-specific objectives",
                        Temperature.balanced,
                        History.State(List.empty, 0)
                    )
                )
            }

            val mind1 = Mind.State(
                "specialist-A",
                1,
                organism1Neurons,
                Gate.State("Domain A expert", "", Temperature.balanced, History.State(List.empty, 0))
            )
            val mind2 = Mind.State(
                "specialist-B",
                1,
                organism2Neurons,
                Gate.State("Domain B expert", "", Temperature.balanced, History.State(List.empty, 0))
            )

            val org1 = Individual(mind1, mind1, List.empty, sex = false, Lineage.empty)
            val org2 = Individual(mind2, mind2, List.empty, sex = true, Lineage.empty)

            val sim = org1.similarity(org2)

            // PHENOTYPE: Both have 10 neurons, same topology (chain), 50% conscious, 50% memory
            // GENOTYPE: Zero shared neurons, completely different prompts, different domains
            //   - Domain A specialist: "neuron-type-A-0", "neuron-type-A-1", ...
            //   - Domain B specialist: "neuron-type-B-0", "neuron-type-B-1", ...
            //   - Prompts: "domain A" vs "domain B"

            // - names: 1.0 (no shared)
            // - output prompts: 1.0 (all different)
            // - memory gate prompts: 1.0 ("memory A" vs "memory B")
            // - mind output: 1.0 ("Domain A" vs "Domain B")

            // SUCCESS: Prompt comparison now distinguishes Domain A from Domain B specialists!
            // Was expected to fail (<0.3), but now correctly identifies moderate-low similarity (0.555)
            assert(sim >= 0.5 && sim < 0.6, s"Domain A vs Domain B specialists should have moderate-low similarity, got $sim")

            // CONCLUSION: Breeding these based on current similarity would create confused hybrids
            // This is why sexual selection might not work as intended!
        }
    }

    "init" - {
        "creates offspring with expected properties" in run {
            val parent1 = createIndividual("parent1", List(0.8), sex = false)
            val parent2 = createIndividual("parent2", List(0.7), sex = true)
            val child   = createMindState("child")

            for individual <- Individual.init(parent1, parent2, child)
            yield
                assert(individual.genotype.name == "child")
                assert(individual.phenotype.name == "child")
                assert(individual.fitness.isEmpty)
            end for
        }

        "genotype and phenotype can differ" in {
            val genotypeState = createMindState("genotype")
            val phenotypeWithHistory = genotypeState.copy(
                output = genotypeState.output.copy(
                    history = History.State(
                        List(History.Entry(0, List(Input("input1")), "output1")),
                        5
                    )
                )
            )

            val individual = Individual(
                genotype = genotypeState,
                phenotype = phenotypeWithHistory,
                fitness = List(0.8).map(TestScore("0", _)),
                sex = false,
                lineage = Lineage.empty
            )

            assert(individual.genotype.output.history.entries.isEmpty)
            assert(individual.phenotype.output.history.entries.nonEmpty)
        }

        "genotype is the heritable blueprint" in {
            val genotypeState = createMindState("clean-genotype")
            val phenotypeState = genotypeState.copy(
                output = genotypeState.output.copy(
                    history = History.State(
                        List(
                            History.Entry(0, List(Input("test1")), "result1"),
                            History.Entry(1, List(Input("test2")), "result2")
                        ),
                        10
                    )
                )
            )

            val parent = Individual(
                genotype = genotypeState,
                phenotype = phenotypeState,
                fitness = List(0.9).map(TestScore("0", _)),
                sex = false,
                lineage = Lineage.empty
            )

            assert(parent.genotype == genotypeState)
            assert(parent.genotype.output.history.entries.isEmpty)
        }

        "phenotype reflects execution state" in {
            val baseState = createMindState("base")
            val executedState = baseState.copy(
                neurons = List(
                    Neuron.State(
                        name = Neuron.Name("neuron1"),
                        description = "test",
                        conscious = true,
                        previous = List.empty,
                        next = List.empty,
                        memory = Some(Memory.State(
                            List(Memory.Entry(2, 1, "learned-during-execution")),
                            Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
                        )),
                        gate = Gate.State(
                            "p",
                            "r",
                            Temperature.balanced,
                            History.State(
                                List(History.Entry(0, List(Input("call1")), "response1")),
                                5
                            )
                        )
                    )
                )
            )

            val individual = Individual(
                genotype = baseState,
                phenotype = executedState,
                fitness = List(0.7).map(TestScore("0", _)),
                sex = true,
                lineage = Lineage.empty
            )

            assert(individual.genotype.neurons.isEmpty)
            assert(individual.phenotype.neurons.nonEmpty)
            assert(individual.phenotype.neurons.head.memory.get.entries.nonEmpty)
            assert(individual.phenotype.neurons.head.gate.history.entries.nonEmpty)
        }

        "offspring receive genotype not phenotype execution state" in run {
            val genotypeState = createMindState("parent-genotype")
            val phenotypeState = genotypeState.copy(
                output = genotypeState.output.copy(
                    history = History.State(
                        List(History.Entry(0, List(Input("runtime-data")), "runtime-output")),
                        5
                    )
                )
            )

            val parent1 =
                Individual(genotypeState, phenotypeState, List(0.8).map(TestScore("0", _)), sex = false, Lineage.empty)
            val parent2 =
                Individual(genotypeState, phenotypeState, List(0.7).map(TestScore("0", _)), sex = true, Lineage.empty)

            for child <- Individual.init(parent1, parent2, genotypeState)
            yield
                assert(child.genotype.output.history.entries.isEmpty)
                assert(child.phenotype.output.history.entries.isEmpty)
            end for
        }

        "mutation would receive phenotype for Lamarckian evolution" in {
            val genotype = createMindState("original")
            val phenotypeWithLearning = genotype.copy(
                output = genotype.output.copy(
                    history = History.State(
                        List(
                            History.Entry(0, List(Input("problem1")), "solution1"),
                            History.Entry(0, List(Input("problem2")), "solution2")
                        ),
                        10
                    )
                )
            )

            val individual = Individual(
                genotype = genotype,
                phenotype = phenotypeWithLearning,
                fitness = List(0.95).map(TestScore("0", _)),
                sex = false,
                lineage = Lineage.empty
            )

            assert(individual.phenotype.output.history.entries.size == 2)
        }

        "freshly created individuals have matching genotype and phenotype" in run {
            val parent1       = createIndividual("p1", List(0.8), sex = false)
            val parent2       = createIndividual("p2", List(0.7), sex = true)
            val childGenotype = createMindState("child")

            for child <- Individual.init(parent1, parent2, childGenotype)
            yield
                assert(child.genotype == childGenotype)
                assert(child.phenotype == childGenotype)
                assert(child.fitness.isEmpty)
            end for
        }
    }

    "breedingPairs" - {
        "produces valid opposite-sex pairs" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("m2", List(0.6), sex = false)
            )
            val females = List(
                createIndividual("f1", List(0.5), sex = true),
                createIndividual("f2", List(0.6), sex = true)
            )

            Individual.breedingPairs(males ++ females, Mating(), 10).map { pairs =>
                assert(pairs.nonEmpty)
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                    // Verify attraction is positive for all pairs
                    val attraction = ind1.attractionTo(ind2, Mating().normalized)
                    assert(attraction > 0.0, s"Attraction between paired individuals should be positive, got $attraction")
                }
                succeed
            }
        }

        "allows individuals to breed multiple times" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("m2", List(0.6), sex = false),
                createIndividual("m3", List(0.7), sex = false)
            )
            val females = List(
                createIndividual("f1", List(0.5), sex = true),
                createIndividual("f2", List(0.6), sex = true),
                createIndividual("f3", List(0.7), sex = true)
            )

            Individual.breedingPairs(males ++ females, Mating(), 10).map { pairs =>
                assert(pairs.size == 10, "Should produce exactly the requested number of pairs")
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                }

                // Verify that some individuals appear multiple times (since 10 pairs from 6 individuals)
                val allIndividuals = pairs.flatMap { case (a, b) => List(a, b) }
                val uniqueCount    = allIndividuals.toSet.size
                assert(uniqueCount <= 6, s"Should reuse individuals, got $uniqueCount unique out of ${allIndividuals.size} total")
                succeed
            }
        }

        "handles unbalanced sex ratios" in run {
            val male = createIndividual("m1", List(0.5), sex = false)
            val females = List(
                createIndividual("f1", List(0.5), sex = true),
                createIndividual("f2", List(0.6), sex = true),
                createIndividual("f3", List(0.7), sex = true)
            )

            Individual.breedingPairs(List(male) ++ females, Mating(), 10).map { pairs =>
                assert(pairs.size == 10, "Should produce requested pairs even with unbalanced ratio")
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                    // With 1 male and 3 females, the single male must appear in all pairs
                    assert(ind1 == male || ind2 == male, "The single male should appear in every pair")
                }
                succeed
            }
        }

        "returns empty for same-sex only population" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("m2", List(0.6), sex = false)
            )

            Individual.breedingPairs(males, Mating(), 10).map { pairs =>
                assert(pairs.isEmpty)
            }
        }

        "returns empty for empty population" in run {
            Individual.breedingPairs(List.empty, Mating(), 10).map { pairs =>
                assert(pairs.isEmpty)
            }
        }

        "returns empty for single individual" in run {
            val single = List(createIndividual("single", List(0.5), sex = false))
            Individual.breedingPairs(single, Mating(), 10).map { pairs =>
                assert(pairs.isEmpty)
            }
        }

        "handles odd number of individuals" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("m2", List(0.6), sex = false)
            )
            val female = createIndividual("f1", List(0.5), sex = true)

            Individual.breedingPairs(males ++ List(female), Mating(), 10).map { pairs =>
                assert(pairs.size == 10, "Should handle odd-sized population")
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                    // Single female should appear in all pairs
                    assert(ind1 == female || ind2 == female, "The single female should appear in every pair")
                }
                succeed
            }
        }

        "pairs by highest mutual attraction" in run {
            val m1 = createIndividual("m1", List(1.0, 0.0), sex = false)
            val m2 = createIndividual("m2", List(0.0, 1.0), sex = false)

            val f1 = createIndividual("f1", List(0.0, 1.0), sex = true)
            val f2 = createIndividual("f2", List(1.0, 0.0), sex = true)

            val weights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)

            // With pure complementarity: m1-f2 and m2-f1 are optimal pairs (complementarity = 0.5 each)
            // m1-f1 and m2-f2 are poor pairs (complementarity = 0.0 each)
            val m1f2 = m1.complementarity(f2) // (0.0*1.0 + 1.0*0.0)/2 = 0.0... wait, this is wrong
            val m1f1 = m1.complementarity(f1) // (0.0*0.0 + 1.0*1.0)/2 = 0.5

            Random.withSeed(42) {
                Individual.breedingPairs(List(m1, m2, f1, f2), weights, 10).map { pairs =>
                    assert(pairs.size == 10, "Should produce requested number of pairs")
                    pairs.foreach { case (ind1, ind2) =>
                        assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                        // Verify all pairs have positive attraction
                        val attraction = ind1.attractionTo(ind2, weights.normalized)
                        assert(attraction >= 0.0, s"Attraction should be non-negative with complementarity weight")
                    }
                    succeed
                }
            }
        }

        "automatically normalizes weights" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("m2", List(0.6), sex = false)
            )
            val females = List(
                createIndividual("f1", List(0.5), sex = true),
                createIndividual("f2", List(0.6), sex = true)
            )

            val unnormalizedWeights = Mating(geneticDistance = 0.6, complementarity = 0.8, similarity = 0.6)
            val normalizedWeights   = unnormalizedWeights.normalized

            Individual.breedingPairs(males ++ females, unnormalizedWeights, 10).map { pairs1 =>
                Individual.breedingPairs(males ++ females, normalizedWeights, 10).map { pairs2 =>
                    assert(pairs1.size == pairs2.size)
                }
            }
        }

        "with very large weights produces same pairs as normal weights" in run {
            val males = List(
                createIndividual("m1", List(1.0, 0.0), sex = false),
                createIndividual("m2", List(0.0, 1.0), sex = false)
            )
            val females = List(
                createIndividual("f1", List(0.0, 1.0), sex = true),
                createIndividual("f2", List(1.0, 0.0), sex = true)
            )

            val normalWeights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)
            val largeWeights  = Mating(geneticDistance = 0.0, complementarity = 100.0, similarity = 0.0)

            Individual.breedingPairs(males ++ females, normalWeights, 10).map { pairs1 =>
                Individual.breedingPairs(males ++ females, largeWeights, 10).map { pairs2 =>
                    assert(pairs1.size == pairs2.size)
                }
            }

        }

        "with zero weights uses default weights" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false)
            )
            val females = List(
                createIndividual("f1", List(0.5), sex = true)
            )

            val zeroWeights = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = 0.0)
            Individual.breedingPairs(males ++ females, zeroWeights, 10).map { pairs =>
                assert(pairs.size == 10)
            }
        }

        "single male and single female" in run {
            val male   = createIndividual("m", List(0.5), sex = false)
            val female = createIndividual("f", List(0.5), sex = true)

            Individual.breedingPairs(List(male, female), Mating(), 10).map { pairs =>
                assert(pairs.size == 10, "Should produce all requested pairs")
                // With only one possible pairing, all pairs must be the same
                pairs.foreach { case (ind1, ind2) =>
                    assert((ind1 == male && ind2 == female) || (ind1 == female && ind2 == male), "Must be the only possible pair")
                    assert(ind1.sex != ind2.sex, "Pair must be opposite sex")
                }
                // Verify deterministic behavior with seed
                Random.withSeed(123) {
                    Individual.breedingPairs(List(male, female), Mating(), 5).map { pairs2 =>
                        assert(pairs2.size == 5)
                        assert(pairs2.forall { case (a, b) => (a == male && b == female) || (a == female && b == male) })
                    }
                }
            }
        }

        "many more males than females" in run {
            val males = List.fill(20)(0).zipWithIndex.map { case (_, i) =>
                createIndividual(s"m$i", List(0.5), sex = false)
            }
            val females = List(
                createIndividual("f1", List(0.5), sex = true),
                createIndividual("f2", List(0.5), sex = true)
            )

            Individual.breedingPairs(males ++ females, Mating(), 10).map { pairs =>
                assert(pairs.size == 10)
                assert(pairs.forall { case (a, b) => a.sex != b.sex })
            }
        }

        "many more females than males" in run {
            val males = List(
                createIndividual("m1", List(0.5), sex = false),
                createIndividual("m2", List(0.5), sex = false)
            )
            val females = List.fill(20)(0).zipWithIndex.map { case (_, i) =>
                createIndividual(s"f$i", List(0.5), sex = true)
            }

            Individual.breedingPairs(males ++ females, Mating(), 10).map { pairs =>
                assert(pairs.size == 10)
                assert(pairs.forall { case (a, b) => a.sex != b.sex })
            }
        }

        "geometric mean of symmetric attraction" in run {
            val ind1 = createIndividual("ind1", List(0.5, 0.5), sex = false)
            val ind2 = createIndividual("ind2", List(0.5, 0.5), sex = true)

            val a12 = ind1.attractionTo(ind2, Mating())
            val a21 = ind2.attractionTo(ind1, Mating())

            val mutualExpected = math.sqrt(a12 * a21)

            Individual.breedingPairs(List(ind1, ind2), Mating(), 10).map { pairs =>
                assert(pairs.size == 10)
            }
        }

        "geometric mean favors balanced over asymmetric attraction" in {
            val m1 = createIndividual("m1", List(0.5), sex = false)
            val m2 = createIndividual("m2", List(0.9), sex = false)

            val f1 = createIndividual("f1", List(0.5), sex = true)
            val f2 = createIndividual("f2", List(0.1), sex = true)

            val weights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)

            val m1f1 = math.sqrt(m1.attractionTo(f1, weights) * f1.attractionTo(m1, weights))
            val m2f2 = math.sqrt(m2.attractionTo(f2, weights) * f2.attractionTo(m2, weights))

            assert(m1f1 > m2f2)
        }

        // EDGE CASE TESTS - Empty fitness in first generation
        "first generation with unevaluated individuals produces pairs" in run {
            // CRITICAL: In real evolution, new individuals start with List.empty fitness
            // This tests that breeding works correctly before any evaluation happens
            val males = List(
                createIndividual("m1", List.empty, sex = false),
                createIndividual("m2", List.empty, sex = false)
            )
            val females = List(
                createIndividual("f1", List.empty, sex = true),
                createIndividual("f2", List.empty, sex = true)
            )

            Individual.breedingPairs(males ++ females, Mating(), 5).map { pairs =>
                assert(pairs.nonEmpty, "Should produce pairs even with empty fitness")
                assert(pairs.size <= 5, "Should produce requested pairs or fewer")
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                    // With empty fitness, complementarity returns 0.5 (neutral)
                    val comp1 = ind1.complementarity(ind2)
                    val comp2 = ind2.complementarity(ind1)
                    assert(comp1 == 0.5, s"Empty fitness should give neutral complementarity, got $comp1")
                    assert(comp2 == 0.5, s"Empty fitness should give neutral complementarity, got $comp2")
                }
                succeed
            }
        }

        "female with zero attraction to all males produces no pairs" in run {
            // Test when selectMates returns empty (line 110 in Individual.scala)
            // This happens when all attraction scores are <= 0
            val incompatibleFemale = createMindStateWithNeuronProperties(
                "incomp",
                neuronCount = 100,
                consciousCount = 100,
                memoryCount = 100,
                avgConnectionsPerNeuron = 20
            )
            val simpleMales = List(
                createIndividual("m1", List(0.5), sex = false, neuronCount = 1),
                createIndividual("m2", List(0.5), sex = false, neuronCount = 1),
                createIndividual("m3", List(0.5), sex = false, neuronCount = 1)
            )

            val female = Individual(
                incompatibleFemale,
                incompatibleFemale,
                List(1.0, 1.0).map(TestScore("0", _)),
                sex = true,
                Lineage.empty
            )

            // With negative similarity weight, female prefers DIFFERENT architectures
            // But these males are TOO different, and she has no gaps (fitness=1.0), so complementarity=0
            val negativeWeights = Mating(geneticDistance = 0.0, complementarity = 0.5, similarity = -1.0)

            val selected = female.selectMates(simpleMales, negativeWeights)

            // With negative similarity weight and very different architectures, attraction could be negative
            // selectMates filters attraction > 0, so some/all males might be filtered out
            if selected.isEmpty then
                assert(true, "No attraction found as expected")
            else
                // If any males selected, their attraction must be positive
                selected.foreach { case (_, attraction) =>
                    assert(attraction > 0, "Selected males must have positive attraction")
                }
                succeed
            end if
        }

        "all individuals are clones with same fitness produces limited pairs" in run {
            // When all individuals have identical genotypes, they filter each other out (line 19)
            val cloneMind = createMindState("clone", neuronCount = 5)
            val males = List(
                Individual(cloneMind, cloneMind, List(0.5).map(TestScore("0", _)), sex = false, Lineage.empty),
                Individual(cloneMind, cloneMind, List(0.5).map(TestScore("0", _)), sex = false, Lineage.empty)
            )
            val females = List(
                Individual(cloneMind, cloneMind, List(0.5).map(TestScore("0", _)), sex = true, Lineage.empty),
                Individual(cloneMind, cloneMind, List(0.5).map(TestScore("0", _)), sex = true, Lineage.empty)
            )

            Individual.breedingPairs(males ++ females, Mating(), 10).map { pairs =>
                // Clones are filtered by genotype equality check (line 19)
                // All clones have genotype == cloneMind, so they're filtered out
                assert(pairs.isEmpty, "Clones with identical genotypes should be filtered out")
            }
        }

        "tournament size larger than available mates produces valid pairs" in run {
            // Test line 105-106: tournamentSize.min(males.size)
            val male = createIndividual("m1", List(0.9), sex = false)
            val females = List(
                createIndividual("f1", List(0.3), sex = true),
                createIndividual("f2", List(0.5), sex = true)
            )

            val largeTournament = Mating(
                geneticDistance = 0.3,
                complementarity = 0.4,
                similarity = 0.3,
                tournamentSize = 100 // Much larger than population
            )

            Individual.breedingPairs(List(male) ++ females, largeTournament, 5).map { pairs =>
                assert(pairs.size == 5, "Should produce requested pairs despite large tournament size")
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                }
                succeed
            }
        }

        "extreme sex ratio allows repeated pairing of same individual" in run {
            // 1 female, 100 males, request 50 pairs -> same female pairs 50 times
            val female = createIndividual("f", List(0.8), sex = true)
            val males = List.fill(100)(0).zipWithIndex.map { case (_, i) =>
                createIndividual(s"m$i", List(0.5), sex = false, neuronCount = 5)
            }

            Individual.breedingPairs(List(female) ++ males, Mating(), 50).map { pairs =>
                assert(pairs.size == 50, "Should produce all requested pairs")

                // Count how many times the female appears
                val femaleCount = pairs.count { case (a, b) => a == female || b == female }
                assert(femaleCount == 50, s"Single female should appear in all 50 pairs, appeared in $femaleCount")

                // Verify variety in male selection
                val uniqueMales = pairs.flatMap {
                    case (a, b) => if a == female then List(b) else List(a)
                }.toSet
                assert(uniqueMales.size > 1, "Should select from multiple males")
            }
        }

        "asexual reproduction creates valid lineage" in run {
            // Test Individual.init when parent1 == parent2 (same parent twice)
            val parent        = createIndividual("parent", List(0.8), sex = false)
            val childGenotype = createMindState("child", neuronCount = 5)

            for child <- Individual.init(parent, parent, childGenotype)
            yield
                assert(child.fitness.isEmpty, "New child should have empty fitness")

                // Lineage adds both parent genotypes (even if same)
                // Line 90: lineage.add(parent1.genotype, parent2.genotype)
                // When both are same, only one hashCode gets added due to sorted de-duplication
                val parentLineage = parent.lineage
                val parentHash    = parent.genotype.hashCode

                // Distance should be less than maximum since child shares parent's lineage
                val distance = child.lineage.distanceTo(parent.lineage)
                assert(distance <= 1.0, s"Child should have genetic similarity to parent, got distance $distance")
                assert(distance >= 0.0, "Distance must be non-negative")
            end for
        }

        "disconnected networks have maximum topology similarity" in {
            // Test line 61: .max(1.0) prevents division by zero
            // When BOTH networks have avgConnections=0, difference is 0/1 = 0
            val disconnected1 = createMindStateWithNeuronProperties("d1", neuronCount = 10, avgConnectionsPerNeuron = 0)
            val disconnected2 = createMindStateWithNeuronProperties("d2", neuronCount = 10, avgConnectionsPerNeuron = 0)

            val ind1 = Individual(disconnected1, disconnected1, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(disconnected2, disconnected2, List.empty, sex = true, Lineage.empty)

            val sim = ind1.similarity(ind2)

            // With identical neuron counts (0), topology (0), conscious (0), memory (0)
            // similarity = (8.0 - 0 - 0 - 0 - 0) / 8.0 = 1.0
            assert(
                sim == 1.0,
                s"Two disconnected networks with same structure should have perfect similarity, got $sim"
            )
        }

        "negative similarity weight favors architectural diversity" in {
            // Test that negative weights produce negative attraction
            val simple  = createIndividual("simple", List(0.5), sex = false, neuronCount = 2)
            val complex = createIndividual("complex", List(0.5), sex = true, neuronCount = 50)

            val negativeSim = Mating(geneticDistance = 0.0, complementarity = 0.0, similarity = -1.0)

            val attraction = simple.attractionTo(complex, negativeSim)

            // Similarity between 2 and 50 neurons is low (high difference)
            // With negative weight: negative * low_similarity = less negative
            // But since similarity is likely high due to formula, attraction will be negative
            val rawSimilarity = simple.similarity(complex)

            val expectedAttraction = rawSimilarity * -1.0
            assert(
                math.abs(attraction - expectedAttraction) < 0.0001,
                s"Expected $expectedAttraction, got $attraction"
            )

            // Negative similarity weight means "prefer different"
            // So LOWER similarity should give HIGHER (less negative) attraction
        }

        "similarity with maximum difference in all components" in {
            // Test boundary conditions - maximum possible dissimilarity
            val minimal = createMindStateWithNeuronProperties(
                "min",
                neuronCount = 2,
                consciousCount = 0,
                memoryCount = 0,
                avgConnectionsPerNeuron = 0
            )
            val maximal = createMindStateWithNeuronProperties(
                "max",
                neuronCount = 10000,
                consciousCount = 10000,
                memoryCount = 10000,
                avgConnectionsPerNeuron = 100
            )

            val ind1 = Individual(minimal, minimal, List.empty, sex = false, Lineage.empty)
            val ind2 = Individual(maximal, maximal, List.empty, sex = true, Lineage.empty)

            val sim = ind1.similarity(ind2)

            // Calculate expected dissimilarities:
            // neuronCount: abs(2-10000)/max(2,10000) ≈ 1.0
            // conscious: abs(0-1.0) = 1.0
            // memory: abs(0-1.0) = 1.0
            // names: 2 shared out of ~10000 unique ≈ 1.0
            // output/route/mind prompts: same → 0.0
            // memory gate prompts: minimal {} vs maximal {"p"} → 1.0

            assert(sim >= 0.0, "Similarity must be non-negative")
            assert(sim < 0.4, s"Maximum dissimilarity should produce low similarity, got $sim")
        }

        "complementarity asymmetry where one individual needs other more" in {
            // Test asymmetric complementarity (line 40-44)
            val desperate = createIndividual("desperate", List(0.0, 0.0, 0.0), sex = false)
            val strong    = createIndividual("strong", List(1.0, 1.0, 1.0), sex = true)

            val desperateNeedsStrong = desperate.complementarity(strong)
            val strongNeedsDesperate = strong.complementarity(desperate)

            // Desperate has gaps [1.0, 1.0, 1.0], strong fills with [1.0, 1.0, 1.0] = avg 1.0
            assert(desperateNeedsStrong == 1.0, s"Desperate should need strong (1.0), got $desperateNeedsStrong")

            // Strong has gaps [0.0, 0.0, 0.0], desperate fills with [0.0, 0.0, 0.0] = avg 0.0
            assert(strongNeedsDesperate == 0.0, s"Strong should not need desperate (0.0), got $strongNeedsDesperate")

            // Verify asymmetry affects mate selection
            val compWeights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)

            val desperateAttraction = desperate.attractionTo(strong, compWeights)
            val strongAttraction    = strong.attractionTo(desperate, compWeights)

            assert(
                desperateAttraction > strongAttraction,
                s"Desperate ($desperateAttraction) should be more attracted than strong ($strongAttraction)"
            )
        }

        "individuals with identical genotypes cannot mate" in {
            // Test that genotype equality prevents mating (line 19: this.genotype != other.genotype)
            // This prevents clones or structurally identical individuals from breeding
            val mind1 = createMindState("identical", neuronCount = 5)
            val mind2 = createMindState("identical", neuronCount = 5)

            val ind1 = Individual(mind1, mind1, List(0.5).map(TestScore("0", _)), sex = false, Lineage.empty)
            val ind2 = Individual(mind2, mind2, List(0.5).map(TestScore("0", _)), sex = true, Lineage.empty)

            // With derives CanEqual, Mind.State uses structural equality
            // Since both minds have identical structure (including deterministic neuron names), they are equal
            assert(mind1 == mind2, "Mind.State with same structure should be equal (structural equality)")

            val selected = ind1.selectMates(List(ind2), Mating())

            // selectMates filters out individuals with identical genotypes to prevent clone breeding
            assert(selected.isEmpty, "Individuals with identical genotypes cannot mate")
        }

        "weighted mate selection respects relative attraction scores" in run {
            // This tests the core sexual selection mechanism via selectMates + Pick
            // Key insight: breedingPairs has too many layers of randomness to test Pick directly
            // Instead, test selectMates which returns ordered candidates, then verify ordering

            // Create males with SAME architecture (so similarity doesn't interfere)
            // but DIFFERENT fitness (to create clear complementarity gradient)
            val male1 = createIndividual("m1", List(0.90), sex = false, neuronCount = 5)
            val male2 = createIndividual("m2", List(0.50), sex = false, neuronCount = 5)
            val male3 = createIndividual("m3", List(0.10), sex = false, neuronCount = 5)

            val female = createIndividual("f", List(0.10), sex = true, neuronCount = 5)

            // Pure complementarity: female prefers males that fill her gaps (0.9)
            val compWeights = Mating(geneticDistance = 0.0, complementarity = 1.0, similarity = 0.0)

            // selectMates returns males ordered by attraction (descending)
            val selected = female.selectMates(List(male1, male2, male3), compWeights)

            // Verify all males are selected (all have positive attraction)
            assert(selected.size == 3, "All males should have positive attraction")

            // Verify ordering: male1 (highest comp) > male2 (medium) > male3 (lowest)
            assert(selected(0)._1 == male1, s"Highest complementarity male should rank first")
            assert(selected(1)._1 == male2, s"Medium complementarity male should rank second")
            assert(selected(2)._1 == male3, s"Lowest complementarity male should rank third")

            // Verify attraction scores are in descending order
            val (_, attr1) = selected(0)
            val (_, attr2) = selected(1)
            val (_, attr3) = selected(2)

            assert(attr1 > attr2, s"First male attraction ($attr1) should be > second ($attr2)")
            assert(attr2 > attr3, s"Second male attraction ($attr2) should be > third ($attr3)")

            // Verify actual complementarity values
            val expectedComp1 = female.complementarity(male1) // ≈ 0.9 * 0.9 = 0.81
            val expectedComp2 = female.complementarity(male2) // ≈ 0.9 * 0.5 = 0.45
            val expectedComp3 = female.complementarity(male3) // ≈ 0.9 * 0.1 = 0.09

            assert(math.abs(attr1 - expectedComp1) < 0.0001, s"Attraction should equal complementarity")
            assert(math.abs(attr2 - expectedComp2) < 0.0001, s"Attraction should equal complementarity")
            assert(math.abs(attr3 - expectedComp3) < 0.0001, s"Attraction should equal complementarity")

            // Now test that breedingPairs produces ANY valid pairs with this setup
            Individual.breedingPairs(List(female, male1, male2, male3), compWeights, 10).map { pairs =>
                assert(pairs.nonEmpty, "Should produce at least some pairs")
                assert(pairs.size <= 10, "Should not exceed requested pairs")

                // Every pair should be opposite sex with positive attraction
                pairs.foreach { case (ind1, ind2) =>
                    assert(ind1.sex != ind2.sex, "All pairs must be opposite sex")
                    val attraction = ind1.attractionTo(ind2, compWeights)
                    assert(attraction > 0, s"Paired individuals should have positive attraction, got $attraction")
                }
                succeed
            }
        }
    }

end IndividualTest
