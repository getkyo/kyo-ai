package kyo.arc2.grid.ops

import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.*
import kyo.arc2.grid.*

type Geometry =
    Geometry.Rotate | Geometry.Flip | Geometry.Transpose | Geometry.Scale | Geometry.Crop | Geometry.CropToBounds | Geometry.Pad | Geometry.Resize | Geometry.Tile

object Geometry:

    @doc("Rotate entire grid by 90, 180, or 270 degrees")
    case class Rotate(
        rotation: Rotation
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            rotation match
                case Rotation.R90 =>
                    val newCells = (0 until grid.width).map { x =>
                        (grid.height - 1 to 0 by -1).map { y =>
                            grid.get(x, y)
                        }.toList
                    }.toList
                    Grid(newCells, grid.config)
                case Rotation.R180 =>
                    Grid(grid.cells.reverse.map(_.reverse), grid.config)
                case Rotation.R270 =>
                    val newCells = (grid.width - 1 to 0 by -1).map { x =>
                        (0 until grid.height).map { y =>
                            grid.get(x, y)
                        }.toList
                    }.toList
                    Grid(newCells, grid.config)
        end apply
    end Rotate

    @doc("Flip entire grid horizontally or vertically")
    case class Flip(
        axis: Axis
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            axis match
                case Axis.Horizontal => Grid(grid.cells.reverse, grid.config)
                case Axis.Vertical   => Grid(grid.cells.map(_.reverse), grid.config)
        end apply
    end Flip

    @doc("Transpose the grid (swap rows and columns)")
    case class Transpose() extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            if grid.cells.isEmpty then Grid(grid.cells, grid.config)
            else
                val newCells = (0 until grid.width).map { x =>
                    (0 until grid.height).map { y =>
                        grid.get(x, y)
                    }.toList
                }.toList
                Grid(newCells, grid.config)
        end apply
    end Transpose

    @doc("Scale entire grid by integer factors")
    case class Scale(
        factorX: Int,
        factorY: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val newCells = grid.cells.flatMap { row =>
                List.fill(factorY)(row.flatMap(color => List.fill(factorX)(color)))
            }
            Grid(newCells, grid.config)
        end apply
    end Scale

    @doc("Extract a rectangular region from the grid")
    case class Crop(
        x: Int,
        y: Int,
        width: Int,
        height: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val newCells = (y until (y + height)).map { rowIdx =>
                (x until (x + width)).map { colIdx =>
                    grid.get(colIdx, rowIdx)
                }.toList
            }.toList
            Grid(newCells, grid.config)
        end apply
    end Crop

    @doc("Crop to the tight bounding box of non-background pixels")
    case class CropToBounds(
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            // Find bounding box of non-background pixels
            val nonBackgroundPixels = (for
                y <- 0 until grid.height
                x <- 0 until grid.width
                if grid.get(x, y) != backgroundColor
            yield (x, y)).toList

            // If no non-background pixels, return 1x1 grid with background
            if nonBackgroundPixels.isEmpty then
                Grid(List(List(backgroundColor)), grid.config)
            else
                val minX = nonBackgroundPixels.map(_._1).min
                val maxX = nonBackgroundPixels.map(_._1).max
                val minY = nonBackgroundPixels.map(_._2).min
                val maxY = nonBackgroundPixels.map(_._2).max

                val width  = maxX - minX + 1
                val height = maxY - minY + 1
                val newCells = (minY until (minY + height)).map { rowIdx =>
                    (minX until (minX + width)).map { colIdx =>
                        grid.get(colIdx, rowIdx)
                    }.toList
                }.toList
                Grid(newCells, grid.config)
            end if
        end apply
    end CropToBounds

    @doc("Add padding around the grid")
    case class Pad(
        top: Int,
        bottom: Int,
        left: Int,
        right: Int,
        fillColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val topPadding    = List.fill(top)(List.fill(grid.width + left + right)(fillColor))
            val bottomPadding = List.fill(bottom)(List.fill(grid.width + left + right)(fillColor))
            val leftPadding   = List.fill(left)(fillColor)
            val rightPadding  = List.fill(right)(fillColor)
            val paddedRows    = grid.cells.map(row => leftPadding ++ row ++ rightPadding)
            Grid(topPadding ++ paddedRows ++ bottomPadding, grid.config)
        end apply
    end Pad

    @doc("Resize grid to specific dimensions")
    case class Resize(
        newWidth: Int,
        newHeight: Int,
        fillColor: Int,
        anchor: Anchor
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val (offsetX, offsetY) = anchor match
                case Anchor.TopLeft => (0, 0)
                case Anchor.Center  => ((newWidth - grid.width) / 2, (newHeight - grid.height) / 2)

            val newCells = (0 until newHeight).map { y =>
                (0 until newWidth).map { x =>
                    val srcX = x - offsetX
                    val srcY = y - offsetY
                    if grid.isValid(srcX, srcY) then grid.get(srcX, srcY)
                    else fillColor
                }.toList
            }.toList
            Grid(newCells, grid.config)
        end apply
    end Resize

    @doc("Tile the grid by repeating it in both directions")
    case class Tile(
        timesX: Int,
        timesY: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val newCells = (0 until grid.height * timesY).map { y =>
                (0 until grid.width * timesX).map { x =>
                    grid.get(x % grid.width, y % grid.height)
                }.toList
            }.toList
            Grid(newCells, grid.config)
        end apply
    end Tile
end Geometry
