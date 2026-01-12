package kyo.arc2.grid

import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.grid.ops.*
import kyo.ren.network.*
import scala.collection.concurrent.TrieMap

object ttt extends KyoApp:
    given Frame = Frame.internal
    run {
        AI.gen[String]("hello")
            .handle(
                GridTool.run(Nil),
                AI.run
            )
    }
end ttt

object GridTool:

    type All = Color | Fill | Geometry | Morphology | Objects | Physics

    case class Call(
        @doc("**Internal** reasoning regarding the generation")
        reasoning: Option[String],
        @doc("Must NOT be empty")
        nonEmptyRequiredGridIds: List[Grid.Id],
        // @doc("Use this field only if there isn't an existing grid")
        // rawGrids: Option[List[List[List[Int]]]],
        // sequentially: Boolean,
        @doc("Required non-empty sequence of operations to apply to the grids")
        nonEmptyRequiredOperations: List[All]
    ) derives Json

    private val gridIdPattern = """g@[0-9a-f]{6}""".r

    def run[A, S](staticGrids: List[Grid])(v: A < S)(using Frame): A < (S & AI) =
        Sync.defer(new TrieMap[Grid.Id, Grid].addAll(staticGrids.map(g => g.id -> g))).map { cache =>
            val tool =
                Tool.init[Call][String, AI](
                    "grid_tool",
                    "Tool to manipulate arg-agi grids. Always specify both the grid ids " +
                        "and operations to execute. They can't be empty."
                ) { call =>
                    Kyo.foreach(call.nonEmptyRequiredGridIds) { id =>
                        Sync.defer {
                            cache.get(id) match
                                case None =>
                                    print("MISS")
                                    AI.fail(s"Invalid grid id: $id")
                                case Some(grid) =>
                                    print("HIT")
                                    grid
                        }
                    }.map { grids =>
                        val results =
                            call.nonEmptyRequiredOperations.foldLeft(grids) { (grids, op) =>
                                op(grids)
                            }
                        Sync.defer {
                            cache.addAll(results.map(g => g.id -> g))
                            results.map(_.toMarkdown).mkString("\n------\n")
                        }
                    }
                }

            v.handle(
                tool.enable,
                Gate.inputsAspect.let(
                    [C] =>
                        (inputs, cont) =>
                            val string = Input.toMarkdown(inputs)

                            val gridIds =
                                gridIdPattern.findAllIn(string)
                                    .distinct.map(Grid.Id(_))
                                    .toList

                            val filtered = gridIds.filter(id => !staticGrids.exists(_.id == id))
                            if filtered.nonEmpty then
                                Sync.defer {
                                    val grids = gridIds.flatMap(cache.get)
                                    val newInput = Input(
                                        List(s"Referenced grids"),
                                        List(grids.map(_.toMarkdown))
                                    )
                                    cont(newInput +: inputs)
                                }
                            else
                                cont(inputs)
                            end if
                )
            )
        }
    end run

end GridTool
