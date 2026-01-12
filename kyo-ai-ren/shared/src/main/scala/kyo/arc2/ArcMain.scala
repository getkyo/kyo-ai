package kyo.arc2

import grid.Grid
import kyo.*
import kyo.Requests.Backend
import kyo.ai.Config
import kyo.ai.json.*
import kyo.debug.Debug
import kyo.ren.*
import kyo.ren.evolution.*
import kyo.ren.evolution.Evolution.TestCase
import kyo.ren.network.Gate
import kyo.ren.network.Input
import scala.io.Source
import sttp.client3.HttpClientKyoBackend
import sttp.client3.SttpBackendOptions

object ArcMain extends KyoApp:

    val key     = "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"
    val version = ArcProblem.Version.V1
    given Frame = Frame.internal

    run {
        AI.run {
            ArcEvolution.init(version).map { (evolution, context) =>
                Loop.whileTrue(!evolution.done) {
                    for
                        s <- evolution.state
                        _ <- AI.withConfig(Config.OpenAI.gpt_5_nano)(evolution.eval(context))

                        evolutionModel =
                            if s.population.generation % 5 == 0 then
                                Config.OpenAI.gpt_5_mini
                            else
                                Config.OpenAI.gpt_5
                        _ <- AI.withConfig(evolutionModel)(evolution.breed(context))
                    yield ()
                }
            }
        }
    }
end ArcMain
