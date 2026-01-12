// package kyo.ren.evolution

// import kyo.*
// import kyo.ren.*
// import kyo.ren.network.*

// class EvolutionLiveTest extends Test:

//     override def timeout = 120.seconds

//     def createMinimalMind(name: String): Mind.State =
//         Mind.State(
//             name = name,
//             maxRecursions = 1,
//             neurons = List.empty,
//             output = Gate.State(
//                 p"""Process the input and provide a brief, accurate response.""",
//                 p"""Be accurate.""",
//                 Temperature.balanced,
//                 History.State(List.empty, 0)
//             )
//         )
//     end createMinimalMind

//     def createIndividual(name: String): Individual =
//         val mindState = createMinimalMind(name)
//         Individual(
//             genotype = mindState,
//             phenotype = mindState,
//             fitness = List.empty,
//             sex = name.hashCode % 2 == 0,
//             lineage = Lineage.empty
//         )
//     end createIndividual

//     "runs basic evolution cycle" in run {
//         val tests = List(
//             Evolution.TestCase("0", "What is 2 + 2?", "4")
//         )

//         val individuals = List(
//             createIndividual("ind1"),
//             createIndividual("ind2")
//         )

//         AI.run {
//             for
//                 evolution <- Evolution.init(Evolution.State(
//                     population = Population.State(
//                         individuals = individuals,
//                         maxSize = 2,
//                         growthFactor = 1.0
//                     ),
//                     tests = tests,
//                     maxGenerations = 2,
//                     eliteFraction = 0.5
//                 ))

//                 ctx <- Context.init(Context.State())

//                 _          <- evolution.run(ctx)
//                 finalState <- evolution.state
//             yield
//                 assert(finalState.population.generation == 2, s"Should run 2 generations: ${finalState.population.generation}")
//                 assert(finalState.population.individuals.forall(_.fitness.nonEmpty), "All individuals should have fitness")
//         }
//     }

//     "demonstrates fitness tracking across generations" in run {
//         val tests = List(
//             Evolution.TestCase("0", "What is 5 + 3?", "8")
//         )

//         val individuals = List(
//             createIndividual("math1"),
//             createIndividual("math2")
//         )

//         AI.run {
//             for
//                 evolution <- Evolution.init(Evolution.State(
//                     population = Population.State(
//                         individuals = individuals,
//                         maxSize = 2,
//                         growthFactor = 1.0
//                     ),
//                     tests = tests,
//                     maxGenerations = 2,
//                     eliteFraction = 0.5
//                 ))

//                 ctx <- Context.init(Context.State())

//                 _         <- evolution.eval(ctx)
//                 initState <- evolution.state
//                 initBest = initState.population.individuals.maxBy(_.fitness.sum)

//                 _          <- evolution.run(ctx)
//                 finalState <- evolution.state
//                 finalBest = finalState.population.individuals.maxBy(_.fitness.sum)
//             yield

//                 assert(
//                     finalBest.fitness.sum >= initBest.fitness.sum - 0.2,
//                     s"Best fitness should not significantly regress: ${initBest.fitness.sum} -> ${finalBest.fitness.sum}"
//                 )
//                 assert(finalState.population.generation == 2, s"Should complete 2 generations: ${finalState.population.generation}")
//         }
//     }

// end EvolutionLiveTest
