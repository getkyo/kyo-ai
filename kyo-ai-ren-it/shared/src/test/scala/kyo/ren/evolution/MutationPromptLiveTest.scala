// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.evolution.Evolution.TestScore
// import kyo.ren.network.*

// class MutationPromptLiveTest extends Test:

//     def createIndividualWithHistory(name: String, executions: List[(String, String)]): Individual =
//         val historyEntries = executions.map { case (input, output) =>
//             History.Entry(List(input), List(""), output)
//         }

//         val neurons = List(
//             Neuron.State(
//                 name = s"${name}_compute",
//                 description = "Computation neuron",
//                 previous = List.empty,
//                 next = List.empty,
//                 conscious = true,
//                 memory = None,
//                 output = Gate.State(
//                     "Perform computation",
//                     "compute",
//                     Temperature.balanced,
//                     History.State(historyEntries, 5)
//                 ),
//                 route = None
//             ),
//             Neuron.State(
//                 name = s"${name}_verify",
//                 description = "Verification neuron",
//                 previous = List(s"${name}_compute"),
//                 next = List.empty,
//                 conscious = true,
//                 memory = None,
//                 output = Gate.State(
//                     "Verify the result",
//                     "verify",
//                     Temperature.balanced,
//                     History.State(historyEntries, 3)
//                 ),
//                 route = None
//             )
//         )

//         val mindState = Mind.State(
//             name = name,
//             maxRecursions = 2,
//             neurons = neurons,
//             output = Gate.State(
//                 "Synthesize final answer",
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

//     def loadPromptMind(using Frame): Mind.State < Sync =
//         Mind.loadDefault("evolution", "mutation", Mutation.Strategy.Prompt(1.0, None), None)

//     "CRITICAL: produces valid Mind.State that can be initialized" in run {
//         AI.run {
//             for
//                 mutator     <- loadPromptMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("test", List(("2+2", "4")))
//                 op <- mutatorInst[Mutation.Op.PromptOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//                 mutatedMind <- Mind.init(mutated)
//                 result      <- mutatedMind[String](List("test input"))
//             yield
//                 assert(result.nonEmpty, "Mutated mind should be able to process input")
//                 assert(mutated.name.nonEmpty, "Mutated mind should have a name")
//         }
//     }

//     "modifies gate prompts based on phenotype patterns" in run {
//         AI.run {
//             for
//                 mutator     <- loadPromptMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("original", List(("input", "output")))
//                 op <- mutatorInst[Mutation.Op.PromptOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val promptsChanged =
//                     mutated.neurons.exists(n =>
//                         individual.phenotype.neurons.find(_.name == n.name).exists(orig =>
//                             n.output.prompt != orig.output.prompt || n.output.reminder != orig.output.reminder
//                         )
//                     ) || mutated.output.prompt != individual.phenotype.output.prompt

//                 assert(promptsChanged, "Prompt mutation should modify at least one gate prompt")
//         }
//     }

//     "preserves network structure while changing prompts" in run {
//         AI.run {
//             for
//                 mutator     <- loadPromptMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("structural", List(("a", "b")))
//                 op <- mutatorInst[Mutation.Op.PromptOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 assert(mutated.neurons.size == individual.phenotype.neurons.size, "Should preserve neuron count")
//                 assert(
//                     mutated.neurons.map(_.name).toSet == individual.phenotype.neurons.map(_.name).toSet,
//                     "Should preserve neuron names"
//                 )

//                 mutated.neurons.zip(individual.phenotype.neurons).foreach { case (mutatedNeuron, origNeuron) =>
//                     if mutatedNeuron.name == origNeuron.name then
//                         assert(mutatedNeuron.previous == origNeuron.previous, "Should preserve connectivity")
//                         assert(mutatedNeuron.next == origNeuron.next, "Should preserve connectivity")
//                         assert(mutatedNeuron.conscious == origNeuron.conscious, "Should preserve conscious flag")
//                         ()
//                 }
//                 succeed
//         }
//     }

//     "maintains all gate prompts as non-empty strings" in run {
//         AI.run {
//             for
//                 mutator     <- loadPromptMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("prompts", List(("test", "result")))
//                 op <- mutatorInst[Mutation.Op.PromptOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 mutated.neurons.foreach { neuron =>
//                     assert(neuron.output.prompt.nonEmpty, s"Neuron ${neuron.name} output prompt should be non-empty")
//                     assert(neuron.output.reminder.nonEmpty, s"Neuron ${neuron.name} output reminder should be non-empty")
//                 }
//                 assert(mutated.output.prompt.nonEmpty, "Output gate prompt should be non-empty")
//                 assert(mutated.output.reminder.nonEmpty, "Output gate reminder should be non-empty")
//                 succeed
//         }
//     }

//     "improves prompt clarity without overfitting to test data" in run {
//         AI.run {
//             for
//                 mutator     <- loadPromptMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithHistory("clarity", List(("47 × 83", "3901")))
//                 op <- mutatorInst[Mutation.Op.PromptOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val prompts            = mutated.neurons.map(_.output.prompt) :+ mutated.output.prompt
//                 val hasHardcodedAnswer = prompts.exists(p => p.contains("3901") || p.contains("47") || p.contains("83"))

//                 assert(!hasHardcodedAnswer, "Prompts should not hardcode specific test case values")
//                 succeed
//         }
//     }

// end MutationPromptLiveTest
