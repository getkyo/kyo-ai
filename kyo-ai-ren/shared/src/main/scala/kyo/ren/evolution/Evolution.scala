package kyo.ren.evolution

import java.util.concurrent.locks.LockSupport
import kyo.*
import kyo.ai.json.*
import kyo.ren.*

final class Evolution(
    init: Evolution.State,
    population: Population,
    path: Option[Path]
):

    def done: Boolean = population.gen >= init.maxGenerations

    def eval(ctx: Context)(using Frame): Unit < AI =
        population.eval(init.tests, ctx.fitness)
            .andThen(save("eval"))

    def breed(ctx: Context)(using Frame): Unit < AI =
        population.breed(init.eliteFraction, ctx.mutation, ctx.crossover, ctx.selection)
            .andThen(save("breed"))

    def state(using Frame): Evolution.State < Sync =
        population.state.map(s => init.copy(population = s))

    private[Evolution] def save(phase: String)(using Frame): Unit < Sync =
        path match
            case None => ()
            case Some(path) =>
                for
                    evolutionState <- state
                    _ <-
                        Path(path, s"gen${population.gen}-$phase.json")
                            .write(Json.encodePretty(evolutionState.population), true)
                yield ()
    end save

end Evolution

object Evolution:

    case class TestCase(testId: TestCase.Id, input: String, expected: String) derives CanEqual

    case class TestScore(testId: TestCase.Id, score: Double) derives CanEqual

    object TestCase:
        type Id = String

    case class State(
        population: Population.State,
        tests: List[TestCase],
        maxGenerations: Int,
        @doc("Fraction of top performers to preserve unchanged (0-1)")
        eliteFraction: Double = 0.3
    ) derives CanEqual, Json:
        require(tests.nonEmpty, "Must have at least one test case")
        require(maxGenerations > 0, s"maxGenerations must be positive, got $maxGenerations")
        require(eliteFraction > 0 && eliteFraction <= 1, s"eliteFraction must be in (0, 1], got $eliteFraction")
    end State

    def init(path: Path, state: State, basePrompt: String)(using Frame): Evolution < AI =
        load(path).map { loaded =>
            val population = loaded.getOrElse(state.population)
            val updated =
                population.copy(individuals =
                    population.individuals.map(i =>
                        i.copy(
                            genotype = i.genotype.copy(basePrompt = basePrompt),
                            phenotype = i.phenotype.copy(basePrompt = basePrompt)
                        )
                    )
                )
            val s = state.copy(population = updated)
            Population.init(s.population).map { population =>
                val evolution = Evolution(s, population, Some(path))
                Kyo.when(loaded.isEmpty)(evolution.save("breed")).andThen(evolution)
            }
        }

    private def load(path: Path)(using Frame): Option[Population.State] < Sync =
        path.exists.map {
            case false => None
            case true =>
                path.list.map { paths =>
                    val checkpoints = paths.map(_.parts.last.toString)
                        .collect {
                            case name if name.matches("gen\\d+-(eval|breed)\\.json") =>
                                val parts = name.stripPrefix("gen").stripSuffix(".json").split("-")
                                (parts(0).toInt, parts(1))
                        }

                    if checkpoints.isEmpty then None
                    else
                        val (gen, phase) =
                            checkpoints.maxBy { case (gen, phase) =>
                                val c =
                                    phase match
                                        case "breed" => 0
                                        case "eval"  => 1
                                (gen, c)
                            }
                        Path(path, s"gen$gen-$phase.json").read
                            .map(json => Some(Json.decode[Population.State](json).getOrThrow))
                    end if
                }
        }

end Evolution
