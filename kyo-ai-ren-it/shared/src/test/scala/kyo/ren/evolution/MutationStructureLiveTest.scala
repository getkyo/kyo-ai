// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.evolution.Evolution.TestScore
// import kyo.ren.network.*

// class MutationStructureLiveTest extends Test:

//     def createIndividualWithHistory(name: String, neuronCount: Int, executions: List[(String, String)]): Individual =
//         val neurons = (0 until neuronCount).map { i =>
//             val historyEntries = executions.map { case (input, output) =>
//                 History.Entry(List(input), List.empty, output)
//             }

//             Neuron.State(
//                 name = s"${name}_neuron_$i",
//                 description = s"Neuron $i in $name",
//                 previous = if i > 0 then List(s"${name}_neuron_${i - 1}") else List.empty,
//                 next = if i < neuronCount - 1 then List(s"${name}_neuron_${i + 1}") else List.empty,
//                 conscious = true,
//                 memory = None,
//                 output = Gate.State(
//                     s"Process step $i",
//                     "process",
//                     Temperature.balanced,
//                     History.State(historyEntries, 3)
//                 ),
//                 route = None
//             )
//         }

//         val mindState = Mind.State(
//             name = name,
//             maxRecursions = 3,
//             neurons = List.from(neurons),
//             output = Gate.State(
//                 "Synthesize result",
//                 "synthesize",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             )
//         )

//         Individual(
//             genotype = mindState,
//             phenotype = mindState,
//             fitness = List(0.7, 0.6).map(TestScore("0", _)),
//             sex = name.hashCode % 2 == 0,
//             lineage = Lineage.empty
//         )
//     end createIndividualWithHistory

//     def loadStructureMind(using Frame): Mind.State < Sync =
//         Mind.loadDefault("evolution", "mutation", Mutation.Strategy.Structure(1.0, None), None)

//     "CRITICAL: produces valid Mind.State that can be initialized" in run {
//         AI.run {
//             for
//                 mutator     <- loadStructureMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("test", 2, List(("input1", "output1")))
//                 op <- mutatorInst[Mutation.Op.StructureOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//                 mutatedMind <- Mind.init(mutated)
//                 result      <- mutatedMind[String](List("test input"))
//             yield
//                 assert(result.nonEmpty, "Mutated mind should be able to process input")
//                 assert(mutated.name.nonEmpty, "Mutated mind should have a name")
//         }
//     }

//     "makes structural modifications to network" in run {
//         AI.run {
//             for
//                 mutator     <- loadStructureMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("original", 3, List(("a", "b")))
//                 op <- mutatorInst[Mutation.Op.StructureOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val structureChanged =
//                     mutated.neurons.size != individual.phenotype.neurons.size ||
//                         mutated.neurons.map(_.name).toSet != individual.phenotype.neurons.map(_.name).toSet ||
//                         mutated.neurons.exists(n =>
//                             individual.phenotype.neurons.find(_.name == n.name).exists(orig =>
//                                 n.previous != orig.previous || n.next != orig.next || n.conscious != orig.conscious
//                             )
//                         )

//                 assert(structureChanged, "Structure mutation should modify network architecture")
//         }
//     }

//     "maintains neuron connectivity validity" in run {
//         AI.run {
//             for
//                 mutator     <- loadStructureMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("connected", 4, List(("x", "y")))
//                 op <- mutatorInst[Mutation.Op.StructureOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val neuronNames = mutated.neurons.map(_.name).toSet
//                 mutated.neurons.foreach { neuron =>
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

//     "preserves core functionality while modifying structure" in run {
//         AI.run {
//             for
//                 mutator     <- loadStructureMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("functional", 3, List(("test", "result")))
//                 op <- mutatorInst[Mutation.Op.StructureOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 assert(mutated.neurons.nonEmpty || mutated.output.prompt.nonEmpty, "Mutated mind should have processing capability")
//                 assert(mutated.maxRecursions > 0, "Should have positive recursion limit")
//         }
//     }

//     "handles mind with no neurons" in run {
//         AI.run {
//             for
//                 mutator     <- loadStructureMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("empty", 0, List.empty)
//                 op <- mutatorInst[Mutation.Op.StructureOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 assert(mutated.output.prompt.nonEmpty, "Should have output gate even with no neurons")
//                 succeed
//         }
//     }

// end MutationStructureLiveTest
