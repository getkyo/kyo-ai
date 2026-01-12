// package kyo.ren.evolution

// import kyo.*
// import kyo.ai.json.*
// import kyo.ren.*
// import kyo.ren.evolution.Evolution.TestScore
// import kyo.ren.network.*

// class MutationParamsLiveTest extends Test:

//     def createIndividualWithParams(name: String, temp: Temperature, maxRec: Int, histSize: Int): Individual =
//         val neurons = List(
//             Neuron.State(
//                 name = s"${name}_neuron",
//                 description = "Processing neuron",
//                 previous = List.empty,
//                 next = List.empty,
//                 conscious = true,
//                 memory = None,
//                 output = Gate.State(
//                     "Process data",
//                     "process",
//                     temp,
//                     History.State(List.empty, histSize)
//                 ),
//                 route = None
//             )
//         )

//         val mindState = Mind.State(
//             name = name,
//             maxRecursions = maxRec,
//             neurons = neurons,
//             output = Gate.State(
//                 "Output result",
//                 "output",
//                 temp,
//                 History.State(List.empty, histSize)
//             )
//         )

//         Individual(
//             genotype = mindState,
//             phenotype = mindState,
//             fitness = List(0.7, 0.6).map(TestScore("0", _)),
//             sex = name.hashCode % 2 == 0,
//             lineage = Lineage.empty
//         )
//     end createIndividualWithParams

//     def loadParamsMind(using Frame): Mind.State < Sync =
//         Mind.loadDefault("evolution", "mutation", Mutation.Strategy.Params(1.0, None), None)

//     "CRITICAL: produces valid Mind.State that can be initialized" in run {
//         AI.run {
//             for
//                 mutator     <- loadParamsMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithParams("test", Temperature.balanced, 3, 5)
//                 op <- mutatorInst[Mutation.Op.ParamsOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//                 mutatedMind <- Mind.init(mutated)
//                 result      <- mutatedMind[String](List("test input"))
//             yield
//                 assert(result.nonEmpty, "Mutated mind should be able to process input")
//                 assert(mutated.name.nonEmpty, "Mutated mind should have a name")
//         }
//     }

//     "adjusts hyperparameters based on phenotype" in run {
//         AI.run {
//             for
//                 mutator     <- loadParamsMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithParams("params", Temperature.balanced, 3, 5)
//                 op <- mutatorInst[Mutation.Op.ParamsOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 val tempChanged = mutated.neurons.exists { n =>
//                     individual.phenotype.neurons.find(_.name == n.name).exists(orig =>
//                         n.output.temperature != orig.output.temperature
//                     )
//                 } || mutated.output.temperature != individual.phenotype.output.temperature

//                 val recursionChanged = mutated.maxRecursions != individual.phenotype.maxRecursions

//                 val historyChanged = mutated.neurons.exists { n =>
//                     individual.phenotype.neurons.find(_.name == n.name).exists(orig =>
//                         n.output.history.maxEntries != orig.output.history.maxEntries
//                     )
//                 } || mutated.output.history.maxEntries != individual.phenotype.output.history.maxEntries

//                 assert(
//                     tempChanged || recursionChanged || historyChanged,
//                     "Parameter mutation should adjust at least one hyperparameter"
//                 )
//         }
//     }

//     "maintains positive maxRecursions" in run {
//         AI.run {
//             for
//                 mutator     <- loadParamsMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithParams("recursion", Temperature.balanced, 3, 3)
//                 op <- mutatorInst[Mutation.Op.ParamsOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 assert(mutated.maxRecursions > 0, s"maxRecursions should be positive: ${mutated.maxRecursions}")
//                 assert(mutated.maxRecursions <= 10, s"maxRecursions should be reasonable: ${mutated.maxRecursions}")
//         }
//     }

//     "keeps temperatures in valid range" in run {
//         AI.run {
//             for
//                 mutator     <- loadParamsMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithParams("temp", Temperature.balanced, 2, 3)
//                 op <- mutatorInst[Mutation.Op.ParamsOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 def checkTemp(temp: Temperature, context: String): Unit =
//                     val value = temp match
//                         case Temperature.Precise(v)     => v
//                         case Temperature.Focused(v)     => v
//                         case Temperature.Balanced(v)    => v
//                         case Temperature.Creative(v)    => v
//                         case Temperature.Exploratory(v) => v

//                     assert(value >= 0.0 && value <= 1.0, s"$context temperature should be in [0.0, 1.0]: $value")
//                     ()
//                 end checkTemp

//                 mutated.neurons.foreach(n => checkTemp(n.output.temperature, s"Neuron ${n.name}"))
//                 checkTemp(mutated.output.temperature, "Output gate")
//                 succeed
//         }
//     }

//     "keeps history sizes non-negative and reasonable" in run {
//         AI.run {
//             for
//                 mutator     <- loadParamsMind
//                 mutatorInst <- Mind.init(mutator)
//                 individual = createIndividualWithParams("history", Temperature.balanced, 2, 5)
//                 op <- mutatorInst[Mutation.Op.ParamsOp](List(Json.encode(individual.phenotype)))
//                 mutated = op(individual.genotype)
//             yield
//                 mutated.neurons.foreach { neuron =>
//                     assert(
//                         neuron.output.history.maxEntries >= 0,
//                         s"Neuron ${neuron.name} history maxEntries should be non-negative"
//                     )
//                     assert(
//                         neuron.output.history.maxEntries <= 20,
//                         s"Neuron ${neuron.name} history maxEntries should be reasonable"
//                     )
//                 }
//                 assert(mutated.output.history.maxEntries >= 0, "Output gate history should be non-negative")
//                 succeed
//         }
//     }

// end MutationParamsLiveTest
