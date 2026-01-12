package kyo.arc2.grid.ops

import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.*
import kyo.arc2.grid.*
import zio.schema.annotation.*

type Color = Color.ReplaceColors | Color.NormalizeColors | Color.InvertColors | Color.BinarizeColors

object Color:

    @doc("Replace colors according to a mapping")
    case class ReplaceColors(
        colorMapping: List[Mapping[Int, Int]]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val map      = colorMapping.map(m => m.from -> m.to).toMap
            val newCells = grid.cells.map(row => row.map(color => map.getOrElse(color, color)))
            Grid(newCells, grid.config)
        end apply
    end ReplaceColors

    @doc("Renumber non-background colors to 1, 2, 3, ...")
    case class NormalizeColors(
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val distinctColors = grid.cells.flatten.distinct.filterNot(_ == backgroundColor).sorted
            val colorMap = distinctColors.zipWithIndex.map { (color, idx) =>
                color -> (idx + 1)
            }.toMap
            val newCells = grid.cells.map(row => row.map(color => colorMap.getOrElse(color, color)))
            Grid(newCells, grid.config)
        end apply
    end NormalizeColors

    @doc("Invert colors (max - current)")
    case class InvertColors(
        maxColor: Int,
        preserveBackground: Boolean,
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val newCells = grid.cells.map(row =>
                row.map(color =>
                    if preserveBackground && color == backgroundColor then backgroundColor
                    else maxColor - color
                )
            )
            Grid(newCells, grid.config)
        end apply
    end InvertColors

    @doc("Convert to binary: non-background becomes 1, background becomes 0")
    case class BinarizeColors(
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val newCells = grid.cells.map(row => row.map(color => if color == backgroundColor then 0 else 1))
            Grid(newCells, grid.config)
        end apply
    end BinarizeColors

end Color
