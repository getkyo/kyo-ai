// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.evolution.Evolution.TestScore
// import kyo.ren.network.*

// class PopulationSeederLiveTest extends Test:

//     def createTemplateMind(): Mind.State =
//         Mind.State(
//             name = "template",
//             maxRecursions = 2,
//             neurons = List(
//                 Neuron.State(
//                     name = "template_neuron",
//                     description = "Template processing neuron",
//                     previous = List.empty,
//                     next = List.empty,
//                     conscious = true,
//                     memory = None,
//                     output = Gate.State(
//                         "Process input",
//                         "process",
//                         Temperature.balanced,
//                         History.State(List.empty, 3)
//                     ),
//                     route = None
//                 )
//             ),
//             output = Gate.State(
//                 "Output result",
//                 "output",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             )
//         )
//     end createTemplateMind

//     def loadSeederMind(using Frame): Mind.State < Sync =
//         Mind.loadDefault("evolution", "population", "seeder", None)

//     "CRITICAL: generates valid Mind.State instances" in run {
//         AI.run {
//             for
//                 seeder     <- loadSeederMind
//                 seederInst <- Mind.init(seeder)
//                 template = createTemplateMind()
//                 seed1    <- seederInst[Mind.State](List(Json.encode(template)))
//                 seedMind <- Mind.init(seed1)
//                 result   <- seedMind[String](List("test input"))
//             yield
//                 assert(result.nonEmpty, "Seeded mind should be able to process input")
//                 assert(seed1.name.nonEmpty, "Seeded mind should have a name")
//         }
//     }

//     "generates diverse Mind.State variations" in run {
//         AI.run {
//             for
//                 seeder     <- loadSeederMind
//                 seederInst <- Mind.init(seeder)
//                 template = createTemplateMind()
//                 seed1 <- seederInst[Mind.State](List(Json.encode(template)))
//                 seed2 <- seederInst[Mind.State](List(Json.encode(template)))
//                 seed3 <- seederInst[Mind.State](List(Json.encode(template)))
//             yield
//                 val neuronCounts  = Set(seed1.neurons.size, seed2.neurons.size, seed3.neurons.size)
//                 val maxRecursions = Set(seed1.maxRecursions, seed2.maxRecursions, seed3.maxRecursions)

//                 val isDiverse = neuronCounts.size > 1 || maxRecursions.size > 1 ||
//                     seed1.output.temperature != seed2.output.temperature ||
//                     seed1.output.temperature != seed3.output.temperature

//                 assert(isDiverse, "Seeder should generate diverse variations across multiple calls")
//         }
//     }

//     "maintains structural validity in seeded minds" in run {
//         AI.run {
//             for
//                 seeder     <- loadSeederMind
//                 seederInst <- Mind.init(seeder)
//                 template = createTemplateMind()
//                 seed <- seederInst[Mind.State](List(Json.encode(template)))
//             yield
//                 val neuronNames = seed.neurons.map(_.name).toSet
//                 seed.neurons.foreach { neuron =>
//                     neuron.previous.foreach { prevName =>
//                         assert(neuronNames.contains(prevName), s"Neuron ${neuron.name} references non-existent previous: $prevName")
//                     }
//                     neuron.next.foreach { nextName =>
//                         assert(neuronNames.contains(nextName), s"Neuron ${neuron.name} references non-existent next: $nextName")
//                     }
//                 }
//                 succeed
//         }
//     }

//     "generates minds with reasonable parameters" in run {
//         AI.run {
//             for
//                 seeder     <- loadSeederMind
//                 seederInst <- Mind.init(seeder)
//                 template = createTemplateMind()
//                 seed <- seederInst[Mind.State](List(Json.encode(template)))
//             yield
//                 assert(seed.maxRecursions > 0 && seed.maxRecursions <= 10, s"maxRecursions should be reasonable: ${seed.maxRecursions}")

//                 seed.neurons.foreach { neuron =>
//                     assert(neuron.output.prompt.nonEmpty, s"Neuron ${neuron.name} should have non-empty prompt")
//                     assert(neuron.output.history.maxEntries >= 0, s"Neuron ${neuron.name} history should be non-negative")
//                 }
//                 assert(seed.output.prompt.nonEmpty, "Output gate should have non-empty prompt")
//                 succeed
//         }
//     }

//     "CANONICAL: seeded mind can undergo mutation and execution (pipeline integration)" in run {
//         AI.run {
//             for

//                 seeder     <- loadSeederMind
//                 seederInst <- Mind.init(seeder)
//                 template = createTemplateMind()
//                 seed <- seederInst[Mind.State](List(Json.encode(template)))

//                 individual = Individual(
//                     genotype = seed,
//                     phenotype = seed,
//                     fitness = List(0.5).map(TestScore("0", _)),
//                     sex = true,
//                     lineage = Lineage.empty
//                 )

//                 mutationState = Mutation.State(
//                     strategies = List(Mutation.Strategy.Prompt(1.0, None)),
//                     rate = 1.0
//                 )
//                 mutation = Mutation.init(mutationState)
//                 mutated <- mutation(individual)

//                 mutatedMind <- Mind.init(mutated)
//                 result      <- mutatedMind[String](List("test input"))
//             yield

//                 assert(seed.name.nonEmpty, "Seeder should produce valid mind")
//                 assert(mutated.name.nonEmpty, "Mutated seeded mind should be valid")
//                 assert(result.nonEmpty, "Mutated seeded mind should be executable")
//                 succeed
//         }
//     }

//     "CANONICAL: seeder produces diverse minds across multiple calls" in run {
//         AI.run {
//             for
//                 seeder     <- loadSeederMind
//                 seederInst <- Mind.init(seeder)
//                 template = createTemplateMind()
//                 seed1 <- seederInst[Mind.State](List(Json.encode(template)))
//                 seed2 <- seederInst[Mind.State](List(Json.encode(template)))
//                 seed3 <- seederInst[Mind.State](List(Json.encode(template)))
//                 seed4 <- seederInst[Mind.State](List(Json.encode(template)))
//                 seed5 <- seederInst[Mind.State](List(Json.encode(template)))
//             yield
//                 val seeds = List(seed1, seed2, seed3, seed4, seed5)

//                 val neuronCounts  = seeds.map(_.neurons.size).toSet
//                 val maxRecursions = seeds.map(_.maxRecursions).toSet
//                 val outputTemps   = seeds.map(_.output.temperature).toSet

//                 val diversityScore = neuronCounts.size + maxRecursions.size + outputTemps.size

//                 assert(
//                     diversityScore >= 3,
//                     s"Seeder should produce diverse minds. Diversity score: $diversityScore (neuronCounts=$neuronCounts, maxRecursions=$maxRecursions, temps=$outputTemps)"
//                 )

//                 val uniqueStructures = seeds.map(s => (s.neurons.size, s.maxRecursions, s.name)).toSet
//                 assert(
//                     uniqueStructures.size >= 3,
//                     s"At least 3 of 5 seeds should have unique structures. Got ${uniqueStructures.size} unique"
//                 )
//         }
//     }

// end PopulationSeederLiveTest
