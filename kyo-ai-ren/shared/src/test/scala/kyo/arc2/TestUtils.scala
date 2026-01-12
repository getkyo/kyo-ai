package kyo.arc2

import kyo.Frame
import kyo.ai.json.Json
import kyo.arc2.grid.*
import org.scalatest.Assertion
import org.scalatest.Assertions.*

object TestUtils:

    def grid(width: Int)(values: Int*): Grid =
        val cells = values.grouped(width).map(_.toList).toList
        Grid(cells)
    end grid

    // ============================================================================
    // Common Pattern Generators
    // ============================================================================

    /** Create an empty grid filled with background color. */
    def emptyGrid(width: Int, height: Int, bgColor: Int = 0): Grid =
        Grid(List.fill(height)(List.fill(width)(bgColor)))

    /** Create a solid grid filled with one color. */
    def solidGrid(width: Int, height: Int, color: Int): Grid =
        Grid(List.fill(height)(List.fill(width)(color)))

    /** Create a grid with a solid square at specified position. */
    def squareGrid(gridSize: Int, squareSize: Int, x: Int, y: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val cells = (0 until gridSize).map { row =>
            (0 until gridSize).map { col =>
                if row >= y && row < y + squareSize && col >= x && col < x + squareSize then color
                else bgColor
            }.toList
        }.toList
        Grid(cells)
    end squareGrid

    /** Create a centered square. */
    def centeredSquare(gridSize: Int, squareSize: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val offset = (gridSize - squareSize) / 2
        squareGrid(gridSize, squareSize, offset, offset, color, bgColor)

    /** Create a grid with a hollow square (frame). */
    def hollowSquareGrid(gridSize: Int, squareSize: Int, x: Int, y: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val cells = (0 until gridSize).map { row =>
            (0 until gridSize).map { col =>
                val inSquare = row >= y && row < y + squareSize && col >= x && col < x + squareSize
                val onEdge   = row == y || row == y + squareSize - 1 || col == x || col == x + squareSize - 1
                if inSquare && onEdge then color
                else bgColor
            }.toList
        }.toList
        Grid(cells)
    end hollowSquareGrid

    /** Create a grid with a horizontal line. */
    def horizontalLine(width: Int, height: Int, row: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val cells = (0 until height).map { r =>
            (0 until width).map { c =>
                if r == row then color else bgColor
            }.toList
        }.toList
        Grid(cells)
    end horizontalLine

    /** Create a grid with a vertical line. */
    def verticalLine(width: Int, height: Int, col: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val cells = (0 until height).map { r =>
            (0 until width).map { c =>
                if c == col then color else bgColor
            }.toList
        }.toList
        Grid(cells)
    end verticalLine

    /** Create a checkerboard pattern. */
    def checkerboard(width: Int, height: Int, color1: Int = 0, color2: Int = 1): Grid =
        val cells = (0 until height).map { row =>
            (0 until width).map { col =>
                if (row + col) % 2 == 0 then color1 else color2
            }.toList
        }.toList
        Grid(cells)
    end checkerboard

    /** Create a grid with an L-shape. */
    def lShape(width: Int, height: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val cells = (0 until height).map { row =>
            (0 until width).map { col =>
                if col == 0 || row == height - 1 then color else bgColor
            }.toList
        }.toList
        Grid(cells)
    end lShape

    /** Create a grid with a plus/cross shape. */
    def crossShape(size: Int, color: Int = 1, bgColor: Int = 0): Grid =
        val mid = size / 2
        val cells = (0 until size).map { row =>
            (0 until size).map { col =>
                if row == mid || col == mid then color else bgColor
            }.toList
        }.toList
        Grid(cells)
    end crossShape

    // ============================================================================
    // Test Data Builders
    // ============================================================================

    /** Fluent grid builder for complex test setups.
      */
    class GridBuilder(private var cells: List[List[Int]] = List.empty):
        def withSize(width: Int, height: Int, fillColor: Int = 0): GridBuilder =
            cells = List.fill(height)(List.fill(width)(fillColor))
            this

        def setPixel(x: Int, y: Int, color: Int): GridBuilder =
            cells = cells.zipWithIndex.map { case (row, rowIdx) =>
                if rowIdx == y then
                    row.zipWithIndex.map { case (c, colIdx) =>
                        if colIdx == x then color else c
                    }
                else row
            }
            this
        end setPixel

        def fillRect(x: Int, y: Int, width: Int, height: Int, color: Int): GridBuilder =
            cells = cells.zipWithIndex.map { case (row, rowIdx) =>
                if rowIdx >= y && rowIdx < y + height then
                    row.zipWithIndex.map { case (c, colIdx) =>
                        if colIdx >= x && colIdx < x + width then color else c
                    }
                else row
            }
            this
        end fillRect

        def build: Grid = Grid(cells)
    end GridBuilder

    def gridBuilder: GridBuilder = new GridBuilder()

    // ============================================================================
    // Debugging Helpers
    // ============================================================================

    /** Print grid with descriptive label (useful in test debugging).
      */
    // def debug(grid: Grid, label: String = "Grid")(using Frame): Grid =
    //     println(s"\n=== $label ===")
    //     println(Json.encode(grid))
    //     println(s"Dimensions: ${grid.width}×${grid.height}")
    //     println(s"Total pixels: ${grid.width * grid.height}")

    //     val histogram = grid.cells.flatten.groupBy(identity).view.mapValues(_.size).toSeq.sortBy(_._1)
    //     println(s"Color histogram: ${histogram.map { case (c, count) => s"$c:$count" }.mkString(", ")}")
    //     println()

    //     grid
    // end debug

    /** Measure and print execution time of an operation.
      */
    def timed[T](label: String)(block: => T): T =
        val start   = System.nanoTime()
        val result  = block
        val elapsed = (System.nanoTime() - start) / 1_000_000.0
        println(f"$label: ${elapsed}%.2f ms")
        result
    end timed

end TestUtils
