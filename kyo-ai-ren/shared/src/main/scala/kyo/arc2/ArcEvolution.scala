package kyo.arc2

import kyo.*
import kyo.ai.Config
import kyo.ren.Mind
import kyo.ren.evolution.*

// object testBest extends KyoApp:
//     given Frame = Frame.internal
//     run {
//         AI.run {
//             for
//                 (e, ctx) <- ArcEvolution.init
//                 s        <- e.state
//                 best = s.population.individuals.maxBy(-_.fitness.map(_.score).sum).genotype
//                 mind  <- Mind.init(best)
//                 tests <- ArcProblem.training
//             yield AI.withConfig(Config.OpenRouter.kimi_k2_thinking.apiKey(ArcMain.key))(
//                 Kyo.foreach(tests)(test => Mind.init(best).map(ArcEval()(_, test)))
//             )
//             end for
//         }
//     }
// end testBest

object ArcEvolution:

    def init(version: ArcProblem.Version)(using Frame): (Evolution, Context) < AI =
        for
            testCases <- ArcProblem.training(version)

            seeds <- ArcSeeds.init

            population =
                Population.State(
                    individuals = List.from(seeds),
                    maxSize = 50,
                    growthFactor = 1.2,
                    maxTestsByIteration = 5
                )
            selection = Selection.init(Selection.State.Pareto)
            mutation  = Mutation.init(Mutation.State())
            crossover <- Crossover.init(Crossover.State())

            basePrompt <- Path(
                "/Users/fwbrasil/workspace/kyo-ai/kyo-ai-ren/shared/src/main/resources/arc/prompt-base.md"
            ).read
        yield
            val fitness = Fitness.init(
                eval = ArcEval(),
                latencyWeight = 0,
                costWeight = 0,
                accuracyWeight = 1
            )
            val context =
                Context(
                    fitness = fitness,
                    selection = selection,
                    mutation = mutation,
                    crossover = crossover
                )
            Evolution.init(
                Path("."),
                Evolution.State(
                    population = population,
                    tests = testCases,
                    maxGenerations = 200,
                    eliteFraction = 0.3
                ),
                "" // basePrompt
            ).map(_ -> context)

end ArcEvolution
