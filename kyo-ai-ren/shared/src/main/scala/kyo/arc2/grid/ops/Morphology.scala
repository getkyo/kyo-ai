package kyo.arc2.grid.ops

import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.*
import kyo.arc2.grid.*

type Morphology = Morphology.Dilate | Morphology.Erode | Morphology.ExtractEdges | Morphology.RemoveIsolatedPixels

object Morphology:

    @doc("Expand objects by adding pixels at boundaries")
    case class Dilate(
        iterations: Int,
        connectivity: Connectivity,
        targetColor: Option[Int]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val neighbors = connectivity match
                case Connectivity.FourConnected  => List((0, 1), (1, 0), (0, -1), (-1, 0))
                case Connectivity.EightConnected => List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

            def dilateOnce(g: Grid): Grid =
                val newCells = g.cells.zipWithIndex.map { (row, y) =>
                    row.zipWithIndex.map { (color, x) =>
                        val shouldDilate = targetColor match
                            case None    => color != 0
                            case Some(c) => color == c

                        if shouldDilate then color
                        else
                            val hasNeighbor = neighbors.exists { (dx, dy) =>
                                val nx = x + dx
                                val ny = y + dy
                                if g.isValid(nx, ny) then
                                    val neighborColor = g.get(nx, ny)
                                    targetColor match
                                        case None    => neighborColor != 0
                                        case Some(c) => neighborColor == c
                                else false
                                end if
                            }
                            if hasNeighbor then
                                targetColor.getOrElse {
                                    neighbors.find { (dx, dy) =>
                                        val nx = x + dx
                                        val ny = y + dy
                                        g.isValid(nx, ny) && g.get(nx, ny) != 0
                                    }.map { (dx, dy) => g.get(x + dx, y + dy) }.getOrElse(color)
                                }
                            else color
                            end if
                        end if
                    }
                }
                Grid(newCells, grid.config)
            end dilateOnce

            def foldDilate(remaining: Int, current: Grid): Grid =
                if remaining == 0 then Grid(current.cells, grid.config)
                else foldDilate(remaining - 1, dilateOnce(current))

            foldDilate(iterations, grid)
        end apply
    end Dilate

    @doc("Shrink objects by removing pixels at boundaries")
    case class Erode(
        iterations: Int,
        connectivity: Connectivity,
        targetColor: Option[Int]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val neighbors = connectivity match
                case Connectivity.FourConnected  => List((0, 1), (1, 0), (0, -1), (-1, 0))
                case Connectivity.EightConnected => List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

            def erodeOnce(g: Grid): Grid =
                val newCells = g.cells.zipWithIndex.map { (row, y) =>
                    row.zipWithIndex.map { (color, x) =>
                        val isTarget = targetColor match
                            case None    => color != 0
                            case Some(c) => color == c

                        if !isTarget then color
                        else
                            val allNeighborsMatch = neighbors.forall { (dx, dy) =>
                                val nx = x + dx
                                val ny = y + dy
                                if g.isValid(nx, ny) then
                                    val neighborColor = g.get(nx, ny)
                                    targetColor match
                                        case None    => neighborColor != 0
                                        case Some(c) => neighborColor == c
                                else false
                                end if
                            }
                            if allNeighborsMatch then color else 0
                        end if
                    }
                }
                Grid(newCells, grid.config)
            end erodeOnce

            def foldErode(remaining: Int, current: Grid): Grid =
                if remaining == 0 then Grid(current.cells, grid.config)
                else foldErode(remaining - 1, erodeOnce(current))

            foldErode(iterations, grid)
        end apply
    end Erode

    @doc("Extract the boundary/outline of objects")
    case class ExtractEdges(
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val neighbors = List((0, 1), (1, 0), (0, -1), (-1, 0))
            val newCells = grid.cells.zipWithIndex.map { (row, y) =>
                row.zipWithIndex.map { (color, x) =>
                    if color == backgroundColor then backgroundColor
                    else
                        val hasBgNeighbor = neighbors.exists { (dx, dy) =>
                            val nx = x + dx
                            val ny = y + dy
                            !grid.isValid(nx, ny) || grid.get(nx, ny) == backgroundColor
                        }
                        if hasBgNeighbor then color else backgroundColor
                }
            }
            Grid(newCells, grid.config)
        end apply
    end ExtractEdges

    @doc("Remove single pixels that have no neighbors")
    case class RemoveIsolatedPixels(
        connectivity: Connectivity
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val neighbors = connectivity match
                case Connectivity.FourConnected  => List((0, 1), (1, 0), (0, -1), (-1, 0))
                case Connectivity.EightConnected => List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

            val newCells = grid.cells.zipWithIndex.map { (row, y) =>
                row.zipWithIndex.map { (color, x) =>
                    if color == 0 then 0
                    else
                        val hasSameColorNeighbor = neighbors.exists { (dx, dy) =>
                            val nx = x + dx
                            val ny = y + dy
                            grid.isValid(nx, ny) && grid.get(nx, ny) == color
                        }
                        if hasSameColorNeighbor then color else 0
                }
            }
            Grid(newCells, grid.config)
        end apply
    end RemoveIsolatedPixels

end Morphology
