package kyo.arc2.grid.ops

import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.*
import kyo.arc2.grid.*

type Physics = Physics.ApplyGravity | Physics.Compact

object Physics:

    @doc("Apply gravity to make objects fall in a direction")
    case class ApplyGravity(
        direction: Direction,
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            direction match
                case Direction.Down =>
                    val newCells = (0 until grid.width).map { x =>
                        val column  = (0 until grid.height).map(y => grid.get(x, y)).toList
                        val nonBg   = column.filterNot(_ == backgroundColor)
                        val bgCount = column.count(_ == backgroundColor)
                        List.fill(bgCount)(backgroundColor) ++ nonBg
                    }.toList
                    Grid(newCells.transpose, grid.config)

                case Direction.Up =>
                    val newCells = (0 until grid.width).map { x =>
                        val column  = (0 until grid.height).map(y => grid.get(x, y)).toList
                        val nonBg   = column.filterNot(_ == backgroundColor)
                        val bgCount = column.count(_ == backgroundColor)
                        nonBg ++ List.fill(bgCount)(backgroundColor)
                    }.toList
                    Grid(newCells.transpose, grid.config)

                case Direction.Right =>
                    val newCells = grid.cells.map { row =>
                        val nonBg   = row.filterNot(_ == backgroundColor)
                        val bgCount = row.count(_ == backgroundColor)
                        List.fill(bgCount)(backgroundColor) ++ nonBg
                    }
                    Grid(newCells, grid.config)

                case Direction.Left =>
                    val newCells = grid.cells.map { row =>
                        val nonBg   = row.filterNot(_ == backgroundColor)
                        val bgCount = row.count(_ == backgroundColor)
                        nonBg ++ List.fill(bgCount)(backgroundColor)
                    }
                    Grid(newCells, grid.config)
        end apply
    end ApplyGravity

    @doc("Remove gaps between objects along an axis")
    case class Compact(
        axis: Axis,
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            axis match
                case Axis.Horizontal =>
                    val newCells = grid.cells.map { row =>
                        val nonBg   = row.filterNot(_ == backgroundColor)
                        val bgCount = row.count(_ == backgroundColor)
                        nonBg ++ List.fill(bgCount)(backgroundColor)
                    }
                    Grid(newCells, grid.config)

                case Axis.Vertical =>
                    val newCells = (0 until grid.width).map { x =>
                        val column  = (0 until grid.height).map(y => grid.get(x, y)).toList
                        val nonBg   = column.filterNot(_ == backgroundColor)
                        val bgCount = column.count(_ == backgroundColor)
                        nonBg ++ List.fill(bgCount)(backgroundColor)
                    }.toList
                    Grid(newCells.transpose, grid.config)
        end apply
    end Compact

end Physics
