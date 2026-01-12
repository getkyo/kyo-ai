package kyo.arc2.grid.ops

import kyo.ai.json.Json
import kyo.ai.json.doc
import kyo.arc2.*
import kyo.arc2.grid.*

type Objects =
    Objects.MoveObjects | Objects.RotateObjects | Objects.FlipObjects | Objects.ScaleObjects | Objects.RecolorObjects |
        Objects.RemoveObjects | Objects.IsolateObject | Objects.CopyObject | Objects.ReplicateObject | Objects.AnalysisParams

object Objects:

    @doc("Move objects by a delta offset")
    case class MoveObjects(
        objectIds: List[Int],
        deltaX: Int,
        deltaY: Int,
        fillBackground: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val objectSet    = objectIds.toSet
            val positionToId = buildPositionToObjectIdMap(grid)

            // Collect object pixels to move
            val objectPixels = positionToId.collect {
                case ((x, y), objId) if objectSet.contains(objId) =>
                    (x, y, grid.get(x, y))
            }.toList

            val positionsToClear = objectPixels.map { case (x, y, _) => (x, y) }.toSet

            // Build new grid: clear originals and place at new positions
            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    val originalCell = grid.get(x, y)

                    // Check if this position should be cleared (original object location)
                    val shouldClear = positionsToClear.contains((x, y))

                    // Check if this position should receive a moved object
                    val movedObject = objectPixels.find { (ox, oy, _) =>
                        ox + deltaX == x && oy + deltaY == y
                    }.map(_._3)

                    movedObject.getOrElse(if shouldClear then fillBackground else originalCell)
                }.toList
            }.toList

            Grid(newCells, grid.config)
        end apply
    end MoveObjects

    @doc("Rotate objects by 90, 180, or 270 degrees")
    case class RotateObjects(
        objectIds: List[Int],
        rotation: Rotation,
        fillBackground: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val objectSet    = objectIds.toSet
            val positionToId = buildPositionToObjectIdMap(grid)

            // Extract all object pixels with their IDs
            val allObjectPixels = objectIds.flatMap { objId =>
                val pixels = positionToId.collect {
                    case ((x, y), id) if id == objId => (x, y)
                }.toList

                if pixels.nonEmpty then
                    val minX = pixels.map(_._1).min
                    val maxX = pixels.map(_._1).max
                    val minY = pixels.map(_._2).min
                    val maxY = pixels.map(_._2).max

                    pixels.flatMap { (x, y) =>
                        val color  = grid.get(x, y)
                        val localX = x - minX
                        val localY = y - minY

                        val (newX, newY) = rotation match
                            case Rotation.R90 =>
                                (minX + (maxY - minY - localY), minY + localX)
                            case Rotation.R180 =>
                                (minX + (maxX - minX - localX), minY + (maxY - minY - localY))
                            case Rotation.R270 =>
                                (minX + localY, minY + (maxX - minX - localX))

                        Some(((newX, newY), color))
                    }
                else List.empty
                end if
            }

            val rotatedPositions = allObjectPixels.toMap
            val positionsToClear = positionToId.collect {
                case ((x, y), id) if objectSet.contains(id) => (x, y)
            }.toSet

            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    val originalCell = grid.get(x, y)
                    rotatedPositions.get((x, y)).getOrElse(
                        if positionsToClear.contains((x, y)) then fillBackground else originalCell
                    )
                }.toList
            }.toList

            Grid(newCells, grid.config)
        end apply
    end RotateObjects

    @doc("Flip objects horizontally or vertically")
    case class FlipObjects(
        objectIds: List[Int],
        axis: Axis,
        fillBackground: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val objectSet    = objectIds.toSet
            val positionToId = buildPositionToObjectIdMap(grid)

            val allObjectPixels = objectIds.flatMap { objId =>
                val pixels = positionToId.collect {
                    case ((x, y), id) if id == objId => (x, y)
                }.toList

                if pixels.nonEmpty then
                    val minX = pixels.map(_._1).min
                    val maxX = pixels.map(_._1).max
                    val minY = pixels.map(_._2).min
                    val maxY = pixels.map(_._2).max

                    val flipped = axis match
                        case Axis.Horizontal =>
                            pixels.map { (x, y) => ((x, minY + (maxY - y)), grid.get(x, y)) }
                        case Axis.Vertical =>
                            pixels.map { (x, y) => ((minX + (maxX - x), y), grid.get(x, y)) }

                    flipped
                else List.empty
                end if
            }

            val flippedPositions = allObjectPixels.toMap
            val positionsToClear = positionToId.collect {
                case ((x, y), id) if objectSet.contains(id) => (x, y)
            }.toSet

            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    val originalCell = grid.get(x, y)
                    flippedPositions.get((x, y)).getOrElse(
                        if positionsToClear.contains((x, y)) then fillBackground else originalCell
                    )
                }.toList
            }.toList

            Grid(newCells, grid.config)
        end apply
    end FlipObjects

    @doc("Scale objects by integer factors")
    case class ScaleObjects(
        objectIds: List[Int],
        factorX: Int,
        factorY: Int,
        fillBackground: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val objectSet    = objectIds.toSet
            val positionToId = buildPositionToObjectIdMap(grid)

            val allObjectPixels = objectIds.flatMap { objId =>
                val pixels = positionToId.collect {
                    case ((x, y), id) if id == objId => (x, y)
                }.toList

                if pixels.nonEmpty then
                    val minX = pixels.map(_._1).min
                    val minY = pixels.map(_._2).min

                    pixels.flatMap { (x, y) =>
                        val color  = grid.get(x, y)
                        val localX = x - minX
                        val localY = y - minY
                        (0 until factorY).flatMap { dy =>
                            (0 until factorX).map { dx =>
                                val newX = minX + localX * factorX + dx
                                val newY = minY + localY * factorY + dy
                                ((newX, newY), color)
                            }
                        }
                    }
                else List.empty
                end if
            }

            val scaledPositions = allObjectPixels.toMap
            val positionsToClear = positionToId.collect {
                case ((x, y), id) if objectSet.contains(id) => (x, y)
            }.toSet

            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    val originalCell = grid.get(x, y)
                    scaledPositions.get((x, y)).getOrElse(
                        if positionsToClear.contains((x, y)) then fillBackground else originalCell
                    )
                }.toList
            }.toList

            Grid(newCells, grid.config)
        end apply
    end ScaleObjects

    @doc("Change colors of objects according to a mapping")
    case class RecolorObjects(
        objectIds: List[Int],
        colorMapping: List[Mapping[Int, Int]]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val objectSet    = objectIds.toSet
            val positionToId = buildPositionToObjectIdMap(grid)
            val map          = colorMapping.map(m => m.from -> m.to).toMap

            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    val cell = grid.get(x, y)
                    positionToId.get((x, y)) match
                        case Some(objId) if objectSet.contains(objId) =>
                            map.getOrElse(cell, cell)
                        case _ =>
                            cell
                    end match
                }.toList
            }.toList
            Grid(newCells, grid.config)
        end apply
    end RecolorObjects

    @doc("Remove objects from the grid")
    case class RemoveObjects(
        objectIds: List[Int],
        fillColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val objectSet    = objectIds.toSet
            val positionToId = buildPositionToObjectIdMap(grid)

            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    positionToId.get((x, y)) match
                        case Some(objId) if objectSet.contains(objId) =>
                            fillColor
                        case _ =>
                            grid.get(x, y)
                }.toList
            }.toList
            Grid(newCells, grid.config)
        end apply
    end RemoveObjects

    @doc("Extract a single object to a new grid with background")
    case class IsolateObject(
        @doc("Object ID (1, 2, 3...) from the analysis")
        objectId: Int,
        backgroundColor: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val positionToId = buildPositionToObjectIdMap(grid)

            val newCells = (0 until grid.height).map { y =>
                (0 until grid.width).map { x =>
                    positionToId.get((x, y)) match
                        case Some(objId) if objId == objectId =>
                            grid.get(x, y)
                        case _ =>
                            backgroundColor
                }.toList
            }.toList

            Grid(newCells, grid.config)
        end apply
    end IsolateObject

    @doc("Copy an object to a new position")
    case class CopyObject(
        @doc("Source object ID (1, 2, 3...) from the analysis")
        sourceObjectId: Int,
        destX: Int,
        destY: Int
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val positionToId = buildPositionToObjectIdMap(grid)

            // Find all pixels with the source object ID
            val pixels = positionToId.collect {
                case ((x, y), objId) if objId == sourceObjectId =>
                    (x, y, grid.get(x, y))
            }.toList

            if pixels.isEmpty then grid
            else
                val minX = pixels.map(_._1).min
                val minY = pixels.map(_._2).min

                val copiedPixels = pixels.map { (x, y, color) =>
                    val offsetX = x - minX
                    val offsetY = y - minY
                    ((destX + offsetX, destY + offsetY), color)
                }.toMap

                val newCells = (0 until grid.height).map { y =>
                    (0 until grid.width).map { x =>
                        copiedPixels.getOrElse((x, y), grid.get(x, y))
                    }.toList
                }.toList

                Grid(newCells, grid.config)
            end if
        end apply
    end CopyObject

    case class Position(x: Int, y: Int)

    @doc("Replicate an object to multiple positions")
    case class ReplicateObject(
        objectId: Int,
        positions: List[Position]
    ) extends Grid.Op.Unary derives CanEqual, Json:
        def apply(grid: Grid) =
            val positionToId = buildPositionToObjectIdMap(grid)

            // Find all pixels with the object ID
            val pixels = positionToId.collect {
                case ((x, y), objId) if objId == objectId =>
                    (x, y, grid.get(x, y))
            }.toList

            if pixels.isEmpty then grid
            else
                val minX = pixels.map(_._1).min
                val minY = pixels.map(_._2).min

                val replicatedPixels = positions.flatMap {
                    case Position(destX, destY) =>
                        pixels.map { (x, y, color) =>
                            val offsetX = x - minX
                            val offsetY = y - minY
                            ((destX + offsetX, destY + offsetY), color)
                        }
                }.toMap

                val newCells = (0 until grid.height).map { y =>
                    (0 until grid.width).map { x =>
                        replicatedPixels.getOrElse((x, y), grid.get(x, y))
                    }.toList
                }.toList

                Grid(newCells, grid.config)
            end if
        end apply
    end ReplicateObject

    // Helper: Build a map from (x,y) position to object ID using connected component labeling
    // This recreates the same labeling that Analysis.init does
    private def buildPositionToObjectIdMap(grid: Grid): Map[(Int, Int), Int] =
        val config       = grid.config
        var visited      = Set[(Int, Int)]()
        var currentLabel = 1
        val positionToId = scala.collection.mutable.Map[(Int, Int), Int]()

        def labelComponent(startX: Int, startY: Int, color: Int): Unit =
            var queue = List((startX, startY))
            while queue.nonEmpty do
                val (x, y) = queue.head
                queue = queue.tail

                if !visited.contains((x, y)) && grid.isValid(x, y) && grid.get(x, y) == color && color != config.backgroundColor then
                    visited = visited + ((x, y))
                    positionToId((x, y)) = currentLabel

                    // Add neighbors based on connectivity
                    val neighbors = config.connectivity match
                        case Connectivity.FourConnected =>
                            List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
                        case Connectivity.EightConnected =>
                            List(
                                (x - 1, y),
                                (x + 1, y),
                                (x, y - 1),
                                (x, y + 1),
                                (x - 1, y - 1),
                                (x - 1, y + 1),
                                (x + 1, y - 1),
                                (x + 1, y + 1)
                            )

                    queue = queue ++ neighbors.filterNot(visited.contains)
                end if
            end while
        end labelComponent

        // Scan grid in same order as Analysis.init
        for
            y <- 0 until grid.height
            x <- 0 until grid.width
        do
            val color = grid.get(x, y)
            if !visited.contains((x, y)) && color != config.backgroundColor then
                labelComponent(x, y, color)
                currentLabel += 1
        end for

        positionToId.toMap
    end buildPositionToObjectIdMap

    @doc("Create a new grid with non-default analysis parameters. Use this to change the grid default analysis.")
    case class AnalysisParams(
        connectivity: Option[Connectivity] = None,
        backgroundColor: Option[Int] = None,
        minPixels: Option[Int] = None
    ) extends Grid.Op:
        def apply(grids: List[Grid]) =
            grids.map { grid =>
                val newConfig = Analysis.Config(
                    connectivity = connectivity.getOrElse(grid.config.connectivity),
                    backgroundColor = backgroundColor.getOrElse(grid.config.backgroundColor),
                    minPixels = minPixels.getOrElse(grid.config.minPixels)
                )
                Grid(grid.cells, newConfig)
            }
        end apply
    end AnalysisParams

end Objects
