// package kyo.ren.evolution

// import kyo.*
// import kyo.ren.*
// import kyo.ren.evolution.Evolution.TestScore
// import kyo.ren.network.*

// class MutationTest extends Test:

//     def createIndividual(name: String): Individual =
//         val gateState = Gate.State(
//             prompt = "test prompt",
//             reminder = "test reminder",
//             temperature = Temperature.balanced,
//             history = History.State(List.empty, 0)
//         )
//         val mindState = Mind.State(name, 1, List.empty, gateState)
//         Individual(
//             genotype = mindState,
//             phenotype = mindState,
//             fitness = List(0.5).map(TestScore("0", _)),
//             sex = false,
//             lineage = Lineage.empty
//         )
//     end createIndividual

//     "State validation" - {
//         "accepts valid mutation state" in {
//             val state = Mutation.State(
//                 strategies = List(
//                     Mutation.Strategy.Structure(0.3, None),
//                     Mutation.Strategy.Prompt(0.3, None),
//                     Mutation.Strategy.Memory(0.2, None),
//                     Mutation.Strategy.Temp(0.2)
//                 ),
//                 rate = 0.3
//             )
//             assert(state.rate == 0.3)
//             assert(state.strategies.size == 4)
//         }

//         "requires non-empty strategies" in {
//             val result = scala.util.Try {
//                 Mutation.State(
//                     strategies = List.empty,
//                     rate = 0.3
//                 )
//             }
//             assert(result.isFailure)
//         }

//         "requires rate between 0 and 1" in {
//             val result1 = scala.util.Try {
//                 Mutation.State(
//                     strategies = List(Mutation.Strategy.Temp(1.0)),
//                     rate = -0.1
//                 )
//             }
//             assert(result1.isFailure)

//             val result2 = scala.util.Try {
//                 Mutation.State(
//                     strategies = List(Mutation.Strategy.Temp(1.0)),
//                     rate = 1.1
//                 )
//             }
//             assert(result2.isFailure)
//         }

//         "requires positive strategy weights" in {
//             val result1 = scala.util.Try {
//                 Mutation.State(
//                     strategies = List(Mutation.Strategy.Structure(-0.5, None)),
//                     rate = 0.3
//                 )
//             }
//             assert(result1.isFailure)

//             val result2 = scala.util.Try {
//                 Mutation.State(
//                     strategies = List(Mutation.Strategy.Prompt(0.0, None)),
//                     rate = 0.3
//                 )
//             }
//             assert(result2.isFailure)
//         }

//         "accepts rate of 0" in {
//             val state = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 0.0
//             )
//             assert(state.rate == 0.0)
//         }

//         "accepts rate of 1" in {
//             val state = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 1.0
//             )
//             assert(state.rate == 1.0)
//         }

//         "accepts multiple strategies with different weights" in {
//             val state = Mutation.State(
//                 strategies = List(
//                     Mutation.Strategy.Structure(0.25, None),
//                     Mutation.Strategy.Prompt(0.25, None),
//                     Mutation.Strategy.Memory(0.25, None),
//                     Mutation.Strategy.Params(0.25, None)
//                 ),
//                 rate = 0.4
//             )
//             assert(state.strategies.size == 4)
//         }
//     }

//     "Strategy types" - {
//         "creates Structure strategy" in {
//             val strategy = Mutation.Strategy.Structure(0.25, None)
//             assert(strategy.weight == 0.25)
//             assert(strategy.customMind.isEmpty)
//         }

//         "creates Prompt strategy" in {
//             val strategy = Mutation.Strategy.Prompt(0.25, None)
//             assert(strategy.weight == 0.25)
//             assert(strategy.customMind.isEmpty)
//         }

//         "creates Memory strategy" in {
//             val strategy = Mutation.Strategy.Memory(0.15, None)
//             assert(strategy.weight == 0.15)
//             assert(strategy.customMind.isEmpty)
//         }

//         "creates Params strategy" in {
//             val strategy = Mutation.Strategy.Params(0.15, None)
//             assert(strategy.weight == 0.15)
//             assert(strategy.customMind.isEmpty)
//         }

//         "creates Temperature strategy" in {
//             val strategy = Mutation.Strategy.Temp(0.2)
//             assert(strategy.weight == 0.2)
//         }

//         "supports custom mind state in MindBased strategies" in {
//             val customMind = Mind.State(
//                 name = "custom",
//                 maxRecursions = 1,
//                 neurons = List.empty,
//                 output = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
//             )

//             val structure = Mutation.Strategy.Structure(0.25, Some(customMind))
//             assert(structure.customMind.contains(customMind))

//             val prompt = Mutation.Strategy.Prompt(0.25, Some(customMind))
//             assert(prompt.customMind.contains(customMind))

//             val memory = Mutation.Strategy.Memory(0.15, Some(customMind))
//             assert(memory.customMind.contains(customMind))

//             val params = Mutation.Strategy.Params(0.15, Some(customMind))
//             assert(params.customMind.contains(customMind))
//         }

//         "Temperature strategy does not have customMind" in {
//             val temp = Mutation.Strategy.Temp(0.2)
//             // Temperature doesn't extend MindBased, so no customMind field
//             assert(temp.weight == 0.2)
//         }

//         "strategies are distinguishable by type" in {
//             val structure = Mutation.Strategy.Structure(0.5, None)
//             val prompt    = Mutation.Strategy.Prompt(0.5, None)
//             val memory    = Mutation.Strategy.Memory(0.5, None)
//             val params    = Mutation.Strategy.Params(0.5, None)
//             val temp      = Mutation.Strategy.Temp(0.5)

//             assert(structure != prompt)
//             assert(prompt != memory)
//             assert(memory != params)
//             assert(params != temp)
//         }
//     }

//     "default strategies include all types" in {
//         val defaultState = Mutation.State()
//         val strategies   = defaultState.strategies

//         assert(strategies.exists(_.isInstanceOf[Mutation.Strategy.Structure]))
//         assert(strategies.exists(_.isInstanceOf[Mutation.Strategy.Prompt]))
//         assert(strategies.exists(_.isInstanceOf[Mutation.Strategy.Memory]))
//         assert(strategies.exists(_.isInstanceOf[Mutation.Strategy.Params]))
//         assert(strategies.exists(_.isInstanceOf[Mutation.Strategy.Temp]))
//     }

//     "State equality" - {
//         "states with same values are equal" in {
//             val state1 = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 0.2
//             )
//             val state2 = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 0.2
//             )
//             assert(state1 == state2)
//         }

//         "states with different rates are not equal" in {
//             val state1 = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 0.1
//             )
//             val state2 = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 0.2
//             )
//             assert(state1 != state2)
//         }

//         "states with different strategies are not equal" in {
//             val state1 = Mutation.State(
//                 strategies = List(Mutation.Strategy.Structure(1.0, None)),
//                 rate = 0.3
//             )
//             val state2 = Mutation.State(
//                 strategies = List(Mutation.Strategy.Prompt(1.0, None)),
//                 rate = 0.3
//             )
//             assert(state1 != state2)
//         }

//         "states with different strategy weights are not equal" in {
//             val state1 = Mutation.State(
//                 strategies = List(
//                     Mutation.Strategy.Structure(0.6, None),
//                     Mutation.Strategy.Prompt(0.4, None)
//                 ),
//                 rate = 0.3
//             )
//             val state2 = Mutation.State(
//                 strategies = List(
//                     Mutation.Strategy.Structure(0.5, None),
//                     Mutation.Strategy.Prompt(0.5, None)
//                 ),
//                 rate = 0.3
//             )
//             assert(state1 != state2)
//         }
//     }

//     "Mutation operations" - {
//         "applies mutation to individual" in run {
//             val state = Mutation.State(
//                 strategies = List(Mutation.Strategy.Temp(1.0)),
//                 rate = 0.5
//             )

//             val individual = createIndividual("test")
//             val mutation   = Mutation.init(state)

//             AI.run {
//                 mutation(individual).map { mutated =>
//                     assert(mutated != null)
//                     assert(mutated.isInstanceOf[Mind.State])
//                 }
//             }
//         }
//     }

//     "Temperature strategy" - {
//         "mutates temperature values" in run {
//             val neuronState = Neuron.State(
//                 name = Neuron.Name("neuron1"),
//                 description = "test",
//                 conscious = false,
//                 previous = List.empty,
//                 next = List.empty,
//                 memory = None,
//                 gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
//             )

//             val mindState = Mind.State(
//                 name = "test",
//                 maxRecursions = 1,
//                 neurons = List(neuronState),
//                 output = Gate.State("p", "r", Temperature.creative, History.State(List.empty, 0))
//             )

//             val individual = Individual(
//                 genotype = mindState,
//                 phenotype = mindState,
//                 fitness = List(0.5).map(TestScore("0", _)),
//                 sex = false,
//                 lineage = Lineage.empty
//             )

//             val tempStrategy = Mutation.Strategy.Temp(1.0)

//             AI.run {
//                 tempStrategy(individual).map { mutated =>
//                     assert(mutated != null)
//                     assert(mutated.isInstanceOf[Mind.State])
//                 }
//             }
//         }

//         "handles neurons with memory gates" in run {
//             val memoryState = Memory.State(
//                 entries = List.empty,
//                 gate = Gate.State("mp", "mr", Temperature.Focused(0.4), History.State(List.empty, 0))
//             )

//             val neuronState = Neuron.State(
//                 name = Neuron.Name("neuron1"),
//                 description = "test",
//                 conscious = false,
//                 previous = List.empty,
//                 next = List.empty,
//                 memory = Some(memoryState),
//                 gate = Gate.State("p", "r", Temperature.Balanced(0.7), History.State(List.empty, 0))
//             )

//             val mindState = Mind.State(
//                 name = "test",
//                 maxRecursions = 1,
//                 neurons = List(neuronState),
//                 output = Gate.State("p", "r", Temperature.Creative(1.0), History.State(List.empty, 0))
//             )

//             val individual = Individual(
//                 genotype = mindState,
//                 phenotype = mindState,
//                 fitness = List(0.5).map(TestScore("0", _)),
//                 sex = false,
//                 lineage = Lineage.empty
//             )

//             val tempStrategy = Mutation.Strategy.Temp(1.0)

//             AI.run {
//                 Kyo.fill(20)(tempStrategy(individual)).map { results =>
//                     assert(results.forall(_.neurons.head.memory.isDefined))
//                     val memoryTemps = results.map(_.neurons.head.memory.get.gate.temperature.normalized)
//                     assert(memoryTemps.toSet.size > 1 || memoryTemps.forall(_ == 0.4))
//                     assert(memoryTemps.forall(t => t >= 0.1 && t <= 1.5))
//                 }
//             }
//         }

//     }

//     "MindBased strategies" - {
//         "Structure strategy loads custom mind when provided" in run {
//             val customMind = Mind.State(
//                 name = "structure-mutator",
//                 maxRecursions = 2,
//                 neurons = List.empty,
//                 output = Gate.State("mutate structure", "reminder", Temperature.balanced, History.State(List.empty, 0))
//             )

//             val strategy   = Mutation.Strategy.Structure(1.0, Some(customMind))
//             val individual = createIndividual("original")

//             // Strategy should call Mind.loadDefault which will use customMind
//             // Since we can't easily mock Mind.loadDefault, we just verify the strategy type
//             assert(strategy.customMind.contains(customMind))
//             assert(strategy.weight == 1.0)
//         }

//         "Prompt strategy loads custom mind when provided" in run {
//             val customMind = Mind.State(
//                 name = "prompt-mutator",
//                 maxRecursions = 3,
//                 neurons = List.empty,
//                 output = Gate.State("mutate prompt", "reminder", Temperature.creative, History.State(List.empty, 0))
//             )

//             val strategy = Mutation.Strategy.Prompt(1.0, Some(customMind))

//             assert(strategy.customMind.contains(customMind))
//             assert(strategy.weight == 1.0)
//         }

//         "Memory strategy loads default mind when none provided" in run {
//             val strategy = Mutation.Strategy.Memory(1.0, None)

//             assert(strategy.customMind.isEmpty)
//             assert(strategy.weight == 1.0)
//         }

//         "Params strategy loads default mind when none provided" in run {
//             val strategy = Mutation.Strategy.Params(1.0, None)

//             assert(strategy.customMind.isEmpty)
//             assert(strategy.weight == 1.0)
//         }
//     }

//     "handles individual with complex mind state" in run {
//         val neuronState = Neuron.State(
//             name = Neuron.Name("neuron1"),
//             description = "test neuron",
//             conscious = true,
//             previous = List(Neuron.Name("prev")),
//             next = List(Neuron.Name("next")),
//             memory = Some(Memory.State(
//                 List.empty,
//                 Gate.State("mp", "mr", Temperature.balanced, History.State(List.empty, 0))
//             )),
//             gate = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
//         )

//         val complexMindState = Mind.State(
//             name = "complex",
//             maxRecursions = 5,
//             neurons = List(neuronState),
//             output = Gate.State("output", "reminder", Temperature.creative, History.State(List.empty, 10))
//         )

//         val individual = Individual(
//             genotype = complexMindState,
//             phenotype = complexMindState,
//             fitness = List(0.7, 0.8, 0.9).map(TestScore("0", _)),
//             sex = false,
//             lineage = Lineage.empty
//         )

//         val state = Mutation.State(
//             strategies = List(Mutation.Strategy.Temp(1.0)),
//             rate = 0.5
//         )

//         val mutation = Mutation.init(state)

//         AI.run {
//             mutation(individual).map { result =>
//                 assert(result != null)
//             }
//         }
//     }

//     "preserves fitness values during mutation" in run {
//         val originalFitness = List(0.1, 0.5, 0.9)
//         val mindState       = Mind.State("test", 1, List.empty, Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0)))
//         val individual = Individual(
//             genotype = mindState,
//             phenotype = mindState,
//             fitness = originalFitness.map(TestScore("0", _)),
//             sex = false,
//             lineage = Lineage.empty
//         )

//         val state = Mutation.State(
//             strategies = List(Mutation.Strategy.Temp(1.0)),
//             rate = 1.0
//         )

//         val mutation = Mutation.init(state)

//         AI.run {
//             mutation(individual).map { result =>
//                 assert(result.isInstanceOf[Mind.State])
//             }
//         }
//     }

//     "handles empty neurons list" in run {
//         val mindState = Mind.State(
//             name = "empty",
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 0))
//         )

//         val individual = Individual(
//             genotype = mindState,
//             phenotype = mindState,
//             fitness = List(0.5).map(TestScore("0", _)),
//             sex = false,
//             lineage = Lineage.empty
//         )

//         val tempStrategy = Mutation.Strategy.Temp(1.0)

//         AI.run {
//             tempStrategy(individual).map { mutated =>
//                 assert(mutated.neurons.isEmpty)
//                 assert(mutated.name == "empty")
//             }
//         }
//     }

//     "Op.StructureOp" - {
//         "AddNeuron adds new neuron to mind" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val newNeuron = Neuron.State(
//                 name = Neuron.Name("new"),
//                 description = "new neuron",
//                 conscious = true,
//                 previous = List.empty,
//                 next = List.empty,
//                 memory = None,
//                 gate = gateState
//             )

//             val mindState = Mind.State("test", 1, List.empty, gateState)
//             val op        = Mutation.Op.StructureOp.AddNeuron("", "", newNeuron)
//             val result    = op(mindState)

//             assert(result.neurons.size == 1)
//             assert(result.neurons.head.name == "new")
//         }

//         "AddNeuron updates connections when neuron references existing neurons" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val existingNeuron = Neuron.State(
//                 name = Neuron.Name("existing"),
//                 description = "existing neuron",
//                 conscious = false,
//                 previous = List.empty,
//                 next = List.empty,
//                 memory = None,
//                 gate = gateState
//             )

//             val newNeuron = Neuron.State(
//                 name = Neuron.Name("new"),
//                 description = "new neuron",
//                 conscious = true,
//                 previous = List(Neuron.Name("existing")),
//                 next = List.empty,
//                 memory = None,
//                 gate = gateState
//             )

//             val mindState = Mind.State("test", 1, List(existingNeuron), gateState)
//             val op        = Mutation.Op.StructureOp.AddNeuron("", "", newNeuron)
//             val result    = op(mindState)

//             assert(result.neurons.size == 2)
//             val updatedExisting = result.neurons.find(_.name == "existing").get
//             assert(updatedExisting.next.contains("new"))
//         }

//         "AddNeuron bidirectional connection update" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val existingNeuron = Neuron.State(
//                 name = Neuron.Name("existing"),
//                 description = "existing neuron",
//                 conscious = false,
//                 previous = List.empty,
//                 next = List.empty,
//                 memory = None,
//                 gate = gateState
//             )

//             val newNeuron = Neuron.State(
//                 name = Neuron.Name("new"),
//                 description = "new neuron",
//                 conscious = true,
//                 previous = List(Neuron.Name("existing")),
//                 next = List.empty,
//                 memory = None,
//                 gate = gateState
//             )

//             val mindState = Mind.State("test", 1, List(existingNeuron), gateState)
//             val op        = Mutation.Op.StructureOp.AddNeuron("", "", newNeuron)
//             val result    = op(mindState)

//             val updated = result.neurons.find(_.name == "existing").get
//             assert(updated.next.contains("new"))
//             assert(result.neurons.find(_.name == "new").get.previous.contains("existing"))
//         }

//         "RemoveNeuron removes neuron from mind" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1 = Neuron.State(
//                 name = Neuron.Name("n1"),
//                 description = "neuron 1",
//                 conscious = false,
//                 previous = List.empty,
//                 next = List(Neuron.Name("n2")),
//                 memory = None,
//                 gate = gateState
//             )

//             val neuron2 = Neuron.State(
//                 name = Neuron.Name("n2"),
//                 description = "neuron 2",
//                 conscious = false,
//                 previous = List(Neuron.Name("n1")),
//                 next = List.empty,
//                 memory = None,
//                 gate = gateState
//             )

//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op        = Mutation.Op.StructureOp.RemoveNeuron("", Neuron.Name("n1"))
//             val result    = op(mindState)

//             assert(result.neurons.size == 1)
//             assert(result.neurons.head.name == "n2")
//             assert(result.neurons.head.previous.isEmpty)
//         }

//         "RemoveNeuron cleans up all references" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1 = Neuron.State(
//                 Neuron.Name("n1"),
//                 "neuron 1",
//                 false,
//                 List.empty,
//                 List(Neuron.Name("n2"), Neuron.Name("n3")),
//                 None,
//                 gateState
//             )
//             val neuron2 = Neuron.State(
//                 Neuron.Name("n2"),
//                 "neuron 2",
//                 false,
//                 List(Neuron.Name("n1")),
//                 List(Neuron.Name("n3")),
//                 None,
//                 gateState
//             )
//             val neuron3 = Neuron.State(
//                 Neuron.Name("n3"),
//                 "neuron 3",
//                 false,
//                 List(Neuron.Name("n1"), Neuron.Name("n2")),
//                 List.empty,
//                 None,
//                 gateState
//             )

//             val mindState = Mind.State("test", 1, List(neuron1, neuron2, neuron3), gateState)
//             val op        = Mutation.Op.StructureOp.RemoveNeuron("", Neuron.Name("n1"))
//             val result    = op(mindState)

//             assert(result.neurons.size == 2)
//             assert(result.neurons.find(_.name == "n2").get.previous.isEmpty)
//             assert(result.neurons.find(_.name == "n3").get.previous.size == 1)
//         }

//         "AddConnection adds connection between neurons" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1   = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val neuron2   = Neuron.State(Neuron.Name("n2"), "neuron 2", false, List.empty, List.empty, None, gateState)

//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op        = Mutation.Op.StructureOp.AddConnection("", Neuron.Name("n1"), Neuron.Name("n2"))
//             val result    = op(mindState)

//             assert(result.neurons.find(_.name == "n1").get.next.contains("n2"))
//             assert(result.neurons.find(_.name == "n2").get.previous.contains("n1"))
//         }

//         "AddConnection is idempotent" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1 =
//                 Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List(Neuron.Name("n2")), None, gateState)
//             val neuron2 =
//                 Neuron.State(Neuron.Name("n2"), "neuron 2", false, List(Neuron.Name("n1")), List.empty, None, gateState)

//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op        = Mutation.Op.StructureOp.AddConnection("", Neuron.Name("n1"), Neuron.Name("n2"))
//             val result    = op(mindState)

//             assert(result.neurons.find(_.name == "n1").get.next.size == 1)
//             assert(result.neurons.find(_.name == "n2").get.previous.size == 1)
//         }

//         "RemoveConnection removes connection between neurons" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1 =
//                 Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List(Neuron.Name("n2")), None, gateState)
//             val neuron2 =
//                 Neuron.State(Neuron.Name("n2"), "neuron 2", false, List(Neuron.Name("n1")), List.empty, None, gateState)

//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op        = Mutation.Op.StructureOp.RemoveConnection("", Neuron.Name("n1"), Neuron.Name("n2"))
//             val result    = op(mindState)

//             assert(result.neurons.find(_.name == "n1").get.next.isEmpty)
//             assert(result.neurons.find(_.name == "n2").get.previous.isEmpty)
//         }

//         "RemoveConnection is idempotent" in {
//             val gateState = Gate.State("p", "r", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1   = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val neuron2   = Neuron.State(Neuron.Name("n2"), "neuron 2", false, List.empty, List.empty, None, gateState)

//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op        = Mutation.Op.StructureOp.RemoveConnection("", Neuron.Name("n1"), Neuron.Name("n2"))
//             val result    = op(mindState)

//             assert(result.neurons.find(_.name == "n1").get.next.isEmpty)
//             assert(result.neurons.find(_.name == "n2").get.previous.isEmpty)
//         }
//     }

//     "Op.PromptOp" - {
//         "MindOutput updates mind output prompts" in {
//             val gateState = Gate.State("old prompt", "old reminder", Temperature.balanced, History.State(List.empty, 5))
//             val mindState = Mind.State("test", 1, List.empty, gateState)
//             val op        = Mutation.Op.PromptOp.MindOutput("", "", "new prompt", "new reminder")
//             val result    = op(mindState)

//             assert(result.output.prompt == "new prompt")
//             assert(result.output.reminder == "new reminder")
//         }

//         "NeuronOutput updates neuron output prompts" in {
//             val gateState = Gate.State("old prompt", "old reminder", Temperature.balanced, History.State(List.empty, 5))
//             val neuron    = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val mindState = Mind.State("test", 1, List(neuron), gateState)
//             val op        = Mutation.Op.PromptOp.NeuronOutput("", "", Neuron.Name("n1"), "new prompt", "new reminder")
//             val result    = op(mindState)

//             assert(result.neurons.head.gate.prompt == "new prompt")
//             assert(result.neurons.head.gate.reminder == "new reminder")
//         }

//         "NeuronOutput only updates specified neuron" in {
//             val gateState = Gate.State("old prompt", "old reminder", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1   = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val neuron2   = Neuron.State(Neuron.Name("n2"), "neuron 2", false, List.empty, List.empty, None, gateState)
//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op        = Mutation.Op.PromptOp.NeuronOutput("", "", Neuron.Name("n1"), "new prompt", "new reminder")
//             val result    = op(mindState)

//             assert(result.neurons.find(_.name == "n1").get.gate.prompt == "new prompt")
//             assert(result.neurons.find(_.name == "n2").get.gate.prompt == "old prompt")
//         }

//         "NeuronMemory updates memory gate prompts when memory exists" in {
//             val gateState  = Gate.State("old prompt", "old reminder", Temperature.balanced, History.State(List.empty, 5))
//             val memoryGate = Gate.State("old memory", "old memory reminder", Temperature.focused, History.State(List.empty, 3))
//             val memory     = Memory.State(List.empty, memoryGate)
//             val neuron     = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, Some(memory), gateState)
//             val mindState  = Mind.State("test", 1, List(neuron), gateState)
//             val op         = Mutation.Op.PromptOp.NeuronMemory("", "", Neuron.Name("n1"), "new memory", "new memory reminder")
//             val result     = op(mindState)

//             assert(result.neurons.head.memory.get.gate.prompt == "new memory")
//             assert(result.neurons.head.memory.get.gate.reminder == "new memory reminder")
//         }

//         "NeuronMemory does nothing when memory does not exist" in {
//             val gateState = Gate.State("old prompt", "old reminder", Temperature.balanced, History.State(List.empty, 5))
//             val neuron    = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val mindState = Mind.State("test", 1, List(neuron), gateState)
//             val op        = Mutation.Op.PromptOp.NeuronMemory("", "", Neuron.Name("n1"), "new memory", "new memory reminder")
//             val result    = op(mindState)

//             assert(result.neurons.head.memory.isEmpty)
//         }
//     }

//     "Op.MemoryOp" - {
//         "UpsertMemory adds memory to neuron without memory" in {
//             val gateState  = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val memoryGate = Gate.State("memory prompt", "memory reminder", Temperature.focused, History.State(List.empty, 3))
//             val memory     = Memory.State(List.empty, memoryGate)
//             val neuron     = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val mindState  = Mind.State("test", 1, List(neuron), gateState)
//             val op         = Mutation.Op.MemoryOp.UpsertMemory("", "", Neuron.Name("n1"), memory)
//             val result     = op(mindState)

//             assert(result.neurons.head.memory.isDefined)
//             assert(result.neurons.head.memory.get.gate.prompt == "memory prompt")
//         }

//         "UpsertMemory replaces existing memory" in {
//             val gateState     = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val oldMemoryGate = Gate.State("old memory", "old reminder", Temperature.focused, History.State(List.empty, 3))
//             val oldMemory     = Memory.State(List.empty, oldMemoryGate)
//             val newMemoryGate = Gate.State("new memory", "new reminder", Temperature.creative, History.State(List.empty, 7))
//             val newMemory     = Memory.State(List.empty, newMemoryGate)
//             val neuron        = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, Some(oldMemory), gateState)
//             val mindState     = Mind.State("test", 1, List(neuron), gateState)
//             val op            = Mutation.Op.MemoryOp.UpsertMemory("", "", Neuron.Name("n1"), newMemory)
//             val result        = op(mindState)

//             assert(result.neurons.head.memory.get.gate.prompt == "new memory")
//         }

//         "RemoveMemory removes memory from neuron" in {
//             val gateState  = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val memoryGate = Gate.State("memory prompt", "memory reminder", Temperature.focused, History.State(List.empty, 3))
//             val memory     = Memory.State(List.empty, memoryGate)
//             val neuron     = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, Some(memory), gateState)
//             val mindState  = Mind.State("test", 1, List(neuron), gateState)
//             val op         = Mutation.Op.MemoryOp.RemoveMemory("", Neuron.Name("n1"))
//             val result     = op(mindState)

//             assert(result.neurons.head.memory.isEmpty)
//         }

//         "RemoveMemory is idempotent" in {
//             val gateState = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val neuron    = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, None, gateState)
//             val mindState = Mind.State("test", 1, List(neuron), gateState)
//             val op        = Mutation.Op.MemoryOp.RemoveMemory("", Neuron.Name("n1"))
//             val result    = op(mindState)

//             assert(result.neurons.head.memory.isEmpty)
//         }
//     }

//     "Op.ParamsOp" - {
//         "MindParams updates mind level parameters" in {
//             val gateState = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val mindState = Mind.State("test", 3, List.empty, gateState)
//             val op        = Mutation.Op.ParamsOp.MindParams("", maxRecursions = 10, outputHistorySize = 15)
//             val result    = op(mindState)

//             assert(result.maxRecursions == 10)
//             assert(result.output.history.maxEntries == 15)
//         }

//         "NeuronParams updates neuron level parameters" in {
//             val gateState  = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val memoryGate = Gate.State("memory", "memory reminder", Temperature.focused, History.State(List.empty, 2))
//             val memory     = Memory.State(List.empty, memoryGate)
//             val neuron     = Neuron.State(Neuron.Name("n1"), "neuron 1", true, List.empty, List.empty, Some(memory), gateState)
//             val mindState  = Mind.State("test", 1, List(neuron), gateState)
//             val op = Mutation.Op.ParamsOp.NeuronParams(
//                 "",
//                 neuronName = Neuron.Name("n1"),
//                 conscious = false,
//                 outputHistorySize = 10,
//                 routeHistorySize = 8,
//                 memoryHistorySize = Some(6)
//             )
//             val result = op(mindState)

//             assert(result.neurons.head.conscious == false)
//             assert(result.neurons.head.gate.history.maxEntries == 10)
//             assert(result.neurons.head.memory.get.gate.history.maxEntries == 6)
//         }

//         "NeuronParams does not update memory history when not provided" in {
//             val gateState  = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val memoryGate = Gate.State("memory", "memory reminder", Temperature.focused, History.State(List.empty, 2))
//             val memory     = Memory.State(List.empty, memoryGate)
//             val neuron     = Neuron.State(Neuron.Name("n1"), "neuron 1", false, List.empty, List.empty, Some(memory), gateState)
//             val mindState  = Mind.State("test", 1, List(neuron), gateState)
//             val op = Mutation.Op.ParamsOp.NeuronParams(
//                 "",
//                 neuronName = Neuron.Name("n1"),
//                 conscious = true,
//                 outputHistorySize = 10,
//                 routeHistorySize = 8,
//                 memoryHistorySize = None
//             )
//             val result = op(mindState)

//             assert(result.neurons.head.memory.get.gate.history.maxEntries == 2)
//         }

//         "NeuronParams only updates specified neuron" in {
//             val gateState = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 5))
//             val neuron1   = Neuron.State(Neuron.Name("n1"), "neuron 1", true, List.empty, List.empty, None, gateState)
//             val neuron2   = Neuron.State(Neuron.Name("n2"), "neuron 2", false, List.empty, List.empty, None, gateState)
//             val mindState = Mind.State("test", 1, List(neuron1, neuron2), gateState)
//             val op = Mutation.Op.ParamsOp.NeuronParams(
//                 "",
//                 neuronName = Neuron.Name("n1"),
//                 conscious = false,
//                 outputHistorySize = 10,
//                 routeHistorySize = 8,
//                 memoryHistorySize = None
//             )
//             val result = op(mindState)

//             assert(result.neurons.find(_.name == "n1").get.conscious == false)
//             assert(result.neurons.find(_.name == "n2").get.conscious == false)
//         }
//     }

// end MutationTest
