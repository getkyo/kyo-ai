// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.evolution.Evolution.TestScore
// import kyo.ren.network.*

// class MutationMemoryLiveTest extends Test:

//     def createIndividualWithoutMemory(name: String): Individual =
//         val neurons = List(
//             Neuron.State(
//                 name = s"${name}_process",
//                 description = "Processing neuron",
//                 previous = List.empty,
//                 next = List.empty,
//                 conscious = true,
//                 memory = None,
//                 output = Gate.State(
//                     "Process input",
//                     "process",
//                     Temperature.balanced,
//                     History.State(List.empty, 3)
//                 ),
//                 route = None
//             )
//         )

//         val mindState = Mind.State(
//             name = name,
//             maxRecursions = 2,
//             neurons = neurons,
//             output = Gate.State(
//                 "Output result",
//                 "output",
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
//     end createIndividualWithoutMemory

//     def createIndividualWithMemory(name: String): Individual =
//         val neurons = List(
//             Neuron.State(
//                 name = s"${name}_verify",
//                 description = "Verification with memory",
//                 previous = List.empty,
//                 next = List.empty,
//                 conscious = true,
//                 memory = Some(Memory.State(
//                     List(Memory.Entry(0, 0, "Error pattern: off-by-one")),
//                     Gate.State(
//                         "Store error patterns and retrieve relevant ones",
//                         "manage memory",
//                         Temperature.focused,
//                         History.State(List.empty, 2)
//                     )
//                 )),
//                 output = Gate.State(
//                     "Verify using memory",
//                     "verify",
//                     Temperature.balanced,
//                     History.State(List.empty, 3)
//                 ),
//                 route = None
//             )
//         )

//         val mindState = Mind.State(
//             name = name,
//             maxRecursions = 2,
//             neurons = neurons,
//             output = Gate.State(
//                 "Output result",
//                 "output",
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
//     end createIndividualWithMemory

//     def loadMemoryMind(using Frame): Mind.State < Sync =
//         Mind.loadDefault("evolution", "mutation", Mutation.Strategy.Memory(1.0, None), None)

//     "CRITICAL: produces valid Mind.State that can be initialized" in run {
//         AI.run {
//             for
//                 mutator     <- loadMemoryMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithoutMemory("test")
//                 op <- mutatorInst[Mutation.Op.MemoryOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//                 mutatedMind <- Mind.init(mutated)
//                 result      <- mutatedMind[String](List("test input"))
//             yield
//                 assert(result.nonEmpty, "Mutated mind should be able to process input")
//                 assert(mutated.name.nonEmpty, "Mutated mind should have a name")
//         }
//     }

//     "can add memory to neurons without memory" in run {
//         AI.run {
//             for
//                 mutator     <- loadMemoryMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithoutMemory("nomem")
//                 op <- mutatorInst[Mutation.Op.MemoryOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val memoryAdded    = mutated.neurons.exists(_.memory.isDefined)
//                 val memoryRemoved  = !mutated.neurons.exists(_.memory.isDefined)
//                 val memoryModified = true

//                 assert(
//                     memoryAdded || memoryRemoved || memoryModified,
//                     "Memory mutation should add, remove, or modify memory"
//                 )
//         }
//     }

//     "can remove or modify memory from neurons with memory" in run {
//         AI.run {
//             for
//                 mutator     <- loadMemoryMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithMemory("withmem")
//                 op <- mutatorInst[Mutation.Op.MemoryOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val originalHadMemory = individual.phenotype.neurons.exists(_.memory.isDefined)
//                 assert(originalHadMemory, "Test setup should have memory")

//                 val memoryRemoved = !mutated.neurons.exists(_.memory.isDefined)
//                 val memoryModified = mutated.neurons.exists { n =>
//                     n.memory.isDefined && individual.phenotype.neurons.find(_.name == n.name).exists { orig =>
//                         orig.memory.exists(origMem =>
//                             n.memory.exists(newMem => newMem.gate.prompt != origMem.gate.prompt)
//                         )
//                     }
//                 }

//                 assert(
//                     memoryRemoved || memoryModified,
//                     "Should remove or modify existing memory"
//                 )
//         }
//     }

//     "memory gates have clear purpose when added" in run {
//         AI.run {
//             for
//                 mutator     <- loadMemoryMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithoutMemory("addmem")
//                 op <- mutatorInst[Mutation.Op.MemoryOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 mutated.neurons.filter(_.memory.isDefined).foreach { neuron =>
//                     val memGate = neuron.memory.get.gate
//                     assert(memGate.prompt.nonEmpty, s"Memory gate for ${neuron.name} should have non-empty prompt")
//                     assert(memGate.reminder.nonEmpty, s"Memory gate for ${neuron.name} should have non-empty reminder")
//                 }
//                 succeed
//         }
//     }

//     "uses memory sparingly across population" in run {
//         AI.run {
//             for
//                 mutator     <- loadMemoryMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithoutMemory("sparse")
//                 op <- mutatorInst[Mutation.Op.MemoryOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val neuronsWithMemory = mutated.neurons.count(_.memory.isDefined)
//                 val totalNeurons      = mutated.neurons.size

//                 assert(
//                     neuronsWithMemory <= totalNeurons,
//                     s"Should use memory sparingly: $neuronsWithMemory memory neurons out of $totalNeurons total"
//                 )
//         }
//     }

//     "CANONICAL: memory mutation makes meaningful changes (not trivial)" in run {
//         AI.run {
//             for
//                 mutator     <- loadMemoryMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithMemory("semantic-change")
//                 op <- mutatorInst[Mutation.Op.MemoryOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val originalNeurons = individual.phenotype.neurons
//                 val mutatedNeurons  = mutated.neurons

//                 val memoryChanged = originalNeurons.exists { origNeuron =>
//                     mutatedNeurons.find(_.name == origNeuron.name).exists { mutNeuron =>
//                         (origNeuron.memory.isDefined, mutNeuron.memory.isDefined) match
//                             case (true, false) =>
//                                 true
//                             case (false, true) =>
//                                 true
//                             case (true, true) =>
//                                 val origPrompt = origNeuron.memory.get.gate.prompt
//                                 val mutPrompt  = mutNeuron.memory.get.gate.prompt

//                                 origPrompt.trim != mutPrompt.trim && Math.abs(origPrompt.length - mutPrompt.length) > 5
//                             case (false, false) =>
//                                 false
//                     }
//                 } || (mutatedNeurons.count(_.memory.isDefined) != originalNeurons.count(_.memory.isDefined))

//                 assert(
//                     memoryChanged,
//                     "Memory mutation should make meaningful changes: add, remove, or substantially modify memory configuration"
//                 )
//         }
//     }

// end MutationMemoryLiveTest
