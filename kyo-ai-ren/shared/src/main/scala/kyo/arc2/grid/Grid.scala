package kyo.arc2.grid

import kyo.*
import kyo.ai.json.Json
import kyo.arc2.grid.ops.*
import zio.schema.{Schema as ZSchema, *}

case class Grid private (
    cells: List[List[Int]],
    config: Analysis.Config
) derives CanEqual, Json:
    // if cells.nonEmpty then
    //     val expectedWidth = cells..length
    //     require(
    //         cells.forall(_.length == expectedWidth),
    //         s"All rows must have the same length. Expected: $expectedWidth, found: ${cells.zipWithIndex.find(_._1.length != expectedWidth).map { case (row, idx) => s"row $idx has length ${row.length}" }}"
    //     )
    // end if

    // require(colors.forall(c => c >= 0 && c <= 9), s"Grid colors must be 0-9. Found: ${colors.filterNot(c => c >= 0 && c <= 9)}")

    def colors: Set[Int] = cells.flatten.toSet

    def width: Int = if cells.isEmpty then 0 else cells.head.length

    def height: Int = cells.length

    def get(x: Int, y: Int): Int =
        if x >= 0 && x < width && y >= 0 && y < height then cells(y)(x)
        else config.backgroundColor // Return background for out-of-bounds

    def isValid(x: Int, y: Int): Boolean =
        x >= 0 && x < width && y >= 0 && y < height

    def transform(f: Int => Int): Grid =
        copy(cells = cells.map(row => row.map(f)))

    def id: Grid.Id = Grid.Id(f"g@${this.hashCode.abs}%09x".take(8))

    val analysis = Analysis.init(this, config)

    def toMarkdown: String = toMarkdown(true)

    def toMarkdown(withAnalysis: Boolean): String =
        val sb = new StringBuilder()

        // Grid ID
        sb.append(s"\n\nGrid ID: $id (${width}x${height})\n")

        // Visual representation
        sb.append("```\n")
        cells.foreach { row =>
            sb.append(row.mkString(" "))
            sb.append("\n")
        }
        sb.append("```\n")

        // Analysis
        if withAnalysis then
            sb.append(analysis.toMarkdown)

        sb.toString()
    end toMarkdown

end Grid

object Grid:

    def apply(cells: List[List[Int]], config: Analysis.Config = Analysis.Config(), maxObjects: Int = 10): Grid =
        def adjustMinPixels(minPixels: Int): Grid =
            val grid     = Grid(cells, config.copy(minPixels = minPixels))
            val analysis = grid.analysis
            if analysis.objects.length <= maxObjects || minPixels > cells.length * cells.head.length / 4 then
                grid
            else
                adjustMinPixels(minPixels + 1)
            end if
        end adjustMinPixels
        adjustMinPixels(1)
    end apply

    abstract class Op:
        def apply(grids: List[Grid]): List[Grid]

    object Op:
        abstract class Unary extends Op:
            def apply(grid: Grid): Grid
            def apply(grids: List[Grid]) =
                grids.map(apply)
        end Unary
    end Op

    opaque type Id = String

    object Id:
        given Json[Id]            = Json[String]
        given CanEqual[Id, Id]    = CanEqual.derived
        def apply(id: String): Id = id
    end Id

end Grid
