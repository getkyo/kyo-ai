package kyo.arc2.grid.ops

import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.*
import kyo.arc2.grid.*

type Fill = Fill.FloodFill | Fill.FloodFill | Fill.FillHoles | Fill.DiagonalPattern | Fill.FillFromDiagonalPattern

object Fill:

    @doc("Fill a connected region with a new color")
    case class FloodFill(
        x: Int,
        y: Int,
        newColor: Int,
        connectivity: Connectivity
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            if !grid.isValid(x, y) then grid
            else
                val targetColor = grid.get(x, y)
                if targetColor == newColor then grid
                else
                    val neighbors = connectivity match
                        case Connectivity.FourConnected  => List((0, 1), (1, 0), (0, -1), (-1, 0))
                        case Connectivity.EightConnected => List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

                    def fill(toVisit: List[(Int, Int)], visited: Set[(Int, Int)], cells: List[List[Int]]): List[List[Int]] =
                        toVisit match
                            case Nil => cells
                            case (px, py) :: rest =>
                                if visited.contains((px, py)) || !grid.isValid(px, py) || grid.cells(py)(px) != targetColor then
                                    fill(rest, visited, cells)
                                else
                                    val newCells     = cells.updated(py, cells(py).updated(px, newColor))
                                    val newNeighbors = neighbors.map { (dx, dy) => (px + dx, py + dy) }
                                    fill(newNeighbors ++ rest, visited + ((px, py)), newCells)

                    Grid(fill(List((x, y)), Set.empty, grid.cells), grid.config)
                end if
        end apply
    end FloodFill

    @doc("Fill a rectangular region with a color")
    case class FillRegion(
        x: Int,
        y: Int,
        width: Int,
        height: Int,
        color: Int
    ) extends Grid.Op.Unary:
        def apply(grid: Grid) =
            val newCells = grid.cells.zipWithIndex.map { (row, rowIdx) =>
                row.zipWithIndex.map { (cellColor, colIdx) =>
                    if colIdx >= x && colIdx < x + width && rowIdx >= y && rowIdx < y + height then color
                    else cellColor
                }
            }
            Grid(newCells, grid.config)
        end apply
    end FillRegion

    @doc("Fill interior holes within objects")
    case class FillHoles(
        @doc("If provided, object IDs from grid.analysis to fill holes within; if None, fill all enclosed regions")
        objectIds: Option[List[Int]],
        @doc("Color to fill holes with; if None, use each object's dominant color")
        fillColor: Option[Int]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val edgePoints =
                (0 until grid.width).flatMap(x => List((x, 0), (x, grid.height - 1))).toList ++
                    (0 until grid.height).flatMap(y => List((0, y), (grid.width - 1, y))).toList

            val targetIds       = objectIds.getOrElse(List.empty)
            val backgroundColor = if targetIds.isEmpty then 0 else targetIds.head - 10

            def floodFillExternal(toVisit: List[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] =
                toVisit match
                    case Nil => visited
                    case (px, py) :: rest =>
                        if visited.contains((px, py)) || !grid.isValid(px, py) then
                            floodFillExternal(rest, visited)
                        else
                            val color = grid.get(px, py)
                            val isBackground = objectIds match
                                case None      => color == backgroundColor
                                case Some(ids) => !ids.contains(color)

                            if !isBackground then
                                floodFillExternal(rest, visited)
                            else
                                val neighbors = List((0, 1), (1, 0), (0, -1), (-1, 0)).map { (dx, dy) => (px + dx, py + dy) }
                                floodFillExternal(neighbors ++ rest, visited + ((px, py)))
                            end if

            val externalRegion = floodFillExternal(edgePoints.distinct, Set.empty)

            val newCells = grid.cells.zipWithIndex.map { (row, rowIdx) =>
                row.zipWithIndex.map { (color, colIdx) =>
                    val isHole = objectIds match
                        case None      => color == backgroundColor && !externalRegion.contains((colIdx, rowIdx))
                        case Some(ids) => !ids.contains(color) && !externalRegion.contains((colIdx, rowIdx))

                    if isHole then fillColor.getOrElse(color)
                    else color
                }
            }
            Grid(newCells, grid.config)
        end apply
    end FillHoles

    @doc("Fill grid with diagonal pattern - each cell (x,y) gets pattern[(x+y) % pattern.length]")
    case class DiagonalPattern(
        pattern: List[Int]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            if pattern.isEmpty then grid
            else
                val newCells = grid.cells.zipWithIndex.map { (row, y) =>
                    row.zipWithIndex.map { (_, x) =>
                        pattern((x + y) % pattern.length)
                    }
                }
                Grid(newCells, grid.config)
            end if
        end apply
    end DiagonalPattern

    @doc("Fill entire grid by extracting and repeating the sparse diagonal pattern from non-background pixels")
    case class FillFromDiagonalPattern(
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            // Extract colors from non-background pixels and their diagonal indices
            val coloredPixels = (for
                y <- 0 until grid.height
                x <- 0 until grid.width
                color = grid.get(x, y)
                if color != backgroundColor
            yield (x, y, color)).toList

            if coloredPixels.isEmpty then grid
            else
                val uniqueColors  = coloredPixels.map(_._3).distinct
                val patternLength = uniqueColors.length
                val diagonalMap = coloredPixels
                    .map { case (x, y, color) => ((x + y) % patternLength) -> color }
                    .toMap
                val pattern = (0 until patternLength).map(i => diagonalMap(i)).toList

                val newCells = grid.cells.zipWithIndex.map { (row, y) =>
                    row.zipWithIndex.map { (_, x) =>
                        pattern((x + y) % pattern.length)
                    }
                }
                Grid(newCells, grid.config)
            end if
        end apply
    end FillFromDiagonalPattern
end Fill
