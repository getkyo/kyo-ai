package kyo.arc2.grid

import kyo.ai.json.Json
import kyo.ai.json.doc

case class Analysis(
    backgroundColor: Int,
    width: Int,
    height: Int,
    colorHistogram: Map[Int, Int],
    symmetries: List[Symmetry],
    labels: List[Label],
    objects: List[ObjectInfo],
    objectGridFormation: Option[(Int, Int)]
) derives CanEqual, Json:
    def toMarkdown: String =
        val sb = new StringBuilder()

        // Grid properties on one line
        val props = List(
            s"Background:$backgroundColor",
            if colorHistogram.nonEmpty then
                s"Colors: ${colorHistogram.toSeq.sortBy(_._1).map { (c, count) => s"$c($count)" }.mkString(" ")}"
            else "",
            if symmetries.nonEmpty then s"Symmetries:${symmetries.mkString(",")}" else "",
            if labels.nonEmpty then s"Labels:${labels.mkString(",")}" else "",
            objectGridFormation.map { (rows, cols) => s"ObjectGrid:${rows}x${cols}" }.getOrElse("")
        ).filter(_.nonEmpty)

        sb.append(s"${props.mkString(" | ")}\n\n")

        // Objects table
        if objects.nonEmpty then
            sb.append(s"Objects (${objects.size}):\n")

            // First pass: calculate column widths
            case class Row(
                id: String,
                pos: String,
                size: String,
                pixels: String,
                density: String,
                color: String,
                touching: String,
                details: String
            )

            val rows = objects.sortBy(_.id).map { obj =>
                val colors = if obj.colorHistogram.nonEmpty then
                    if obj.colorHistogram.size == 1 then
                        obj.colorHistogram.head._1.toString
                    else
                        obj.colorHistogram.toSeq.sortBy(_._1).map { (c, count) => s"$c($count)" }.mkString(" ")
                else "-"

                val touchingStr = if obj.touchingObjectIds.nonEmpty then obj.touchingObjectIds.sorted.mkString(",") else "-"

                // Build details column with all variable-length info
                val details = List(
                    if obj.shape.nonEmpty then Some(s"Shape:${obj.shape.mkString(",")}") else None,
                    if obj.symmetry.nonEmpty then Some(s"Symmetry:${obj.symmetry.mkString(",")}") else None,
                    if obj.position.nonEmpty then Some(s"Position:${obj.position.mkString(",")}") else None
                ).flatten.mkString("; ")

                val detailsStr = if details.nonEmpty then details else "-"

                Row(
                    obj.id.toString,
                    s"(${obj.x},${obj.y})",
                    s"${obj.width}x${obj.height}",
                    obj.pixelCount.toString,
                    f"${obj.density}%.2f",
                    colors,
                    touchingStr,
                    detailsStr
                )
            }

            // Calculate max widths
            val idW       = math.max(2, rows.map(_.id.length).max)
            val posW      = math.max(6, rows.map(_.pos.length).max)
            val sizeW     = math.max(4, rows.map(_.size.length).max)
            val pixelsW   = math.max(6, rows.map(_.pixels.length).max)
            val densityW  = math.max(7, rows.map(_.density.length).max)
            val colorW    = math.max(5, rows.map(_.color.length).max)
            val touchingW = math.max(8, rows.map(_.touching.length).max)

            // Print header
            sb.append(
                s"| ${"ID".padTo(idW, ' ')} | ${"Center".padTo(posW, ' ')} | ${"Size".padTo(sizeW, ' ')} | ${"Pixels".padTo(pixelsW, ' ')} | ${"Density".padTo(densityW, ' ')} | ${"Color".padTo(colorW, ' ')} | ${"Touching".padTo(touchingW, ' ')} | Details\n"
            )
            sb.append(
                s"|${"-" * (idW + 2)}|${"-" * (posW + 2)}|${"-" * (sizeW + 2)}|${"-" * (pixelsW + 2)}|${"-" * (densityW + 2)}|${"-" * (colorW + 2)}|${"-" * (touchingW + 2)}|:--------\n"
            )

            // Print rows
            rows.foreach { row =>
                sb.append(
                    s"| ${row.id.padTo(idW, ' ')} | ${row.pos.padTo(posW, ' ')} | ${row.size.padTo(sizeW, ' ')} | ${row.pixels.padTo(pixelsW, ' ')} | ${row.density.padTo(densityW, ' ')} | ${row.color.padTo(colorW, ' ')} | ${row.touching.padTo(touchingW, ' ')} | ${row.details}\n"
                )
            }
        end if

        sb.toString()
    end toMarkdown
end Analysis

object Analysis:

    case class Config(
        connectivity: Connectivity = Connectivity.FourConnected,
        backgroundColor: Int = 0,
        minPixels: Int = 1
    ) derives CanEqual, Json:
        require(backgroundColor >= 0 && backgroundColor <= 9, s"Background color must be 0-9. Found: $backgroundColor")
        require(minPixels >= 1, s"Minimum size must be >= 1. Found: $minPixels")
    end Config

    def init(grid: Grid, config: Analysis.Config): Analysis =
        def processPixels(
            positions: List[(Int, Int)],
            visited: Set[(Int, Int)],
            objects: List[(Int, List[(Int, Int, Int)])],
            nextLabel: Int
        ): List[(Int, List[(Int, Int, Int)])] =
            positions match
                case Nil => objects
                case (x, y) :: rest =>
                    val color = grid.get(x, y)
                    if visited.contains((x, y)) || color == config.backgroundColor then
                        processPixels(rest, visited, objects, nextLabel)
                    else
                        val (newVisited, pixels) = labelComponent(
                            grid,
                            x,
                            y,
                            color,
                            config.connectivity,
                            config.backgroundColor,
                            visited
                        )
                        val newObjects = if pixels.length >= config.minPixels then
                            (nextLabel, pixels) +: objects
                        else objects
                        val newLabel = if pixels.length >= config.minPixels then nextLabel + 1 else nextLabel
                        processPixels(rest, newVisited, newObjects, newLabel)
                    end if
        end processPixels

        val allPositions = (for
            y <- 0 until grid.height
            x <- 0 until grid.width
        yield (x, y)).toList

        val objects = processPixels(allPositions, Set.empty, List.empty, 1)

        // First pass: create objects without neighbors
        val objectInfosWithoutNeighbors = objects.reverse.map { (label, pixels) =>
            analyzeObject(grid, label, pixels)
        }

        // Second pass: compute touching objects and alignment for each object
        val objectInfos = objectInfosWithoutNeighbors.map { obj =>
            val touching         = computeTouchingObjects(grid, obj, objectInfosWithoutNeighbors, config.connectivity)
            val alignmentPos     = computeAlignmentPositions(obj, objectInfosWithoutNeighbors)
            val updatedPositions = obj.position ++ alignmentPos
            obj.copy(touchingObjectIds = touching, position = updatedPositions)
        }

        // Compute grid-level statistics
        val allColors   = grid.cells.flatten
        val colorCounts = allColors.groupBy(identity).view.mapValues(_.size).toMap

        def checkSymmetry(sym: Symmetry): Boolean = sym match
            case Symmetry.Vertical =>
                grid.cells.forall(row => row == row.reverse)
            case Symmetry.Horizontal =>
                grid.cells == grid.cells.reverse
            case Symmetry.Diagonal =>
                (0 until grid.height).forall { y =>
                    (0 until grid.width).forall { x =>
                        if x < grid.height && y < grid.width then
                            grid.get(x, y) == grid.get(y, x)
                        else true
                    }
                }
            case Symmetry.AntiDiagonal =>
                (0 until grid.height).forall { y =>
                    (0 until grid.width).forall { x =>
                        val mirrorX = grid.width - 1 - x
                        val mirrorY = grid.height - 1 - y
                        if mirrorY < grid.height && mirrorX < grid.width then
                            grid.get(x, y) == grid.get(mirrorX, mirrorY)
                        else true
                    }
                }

        val gridSymmetries = List(
            Symmetry.Vertical,
            Symmetry.Horizontal,
            Symmetry.Diagonal,
            Symmetry.AntiDiagonal
        ).filter(checkSymmetry)

        def findPeriod(axis: String): Option[Int] =
            val length = if axis == "x" then grid.width else grid.height
            (1 until length).find { period =>
                if length % period == 0 then
                    val repetitions = length / period
                    (0 until repetitions - 1).forall { rep =>
                        if axis == "x" then
                            (0 until grid.height).forall { y =>
                                (0 until period).forall { x =>
                                    grid.get(x, y) == grid.get(x + period * (rep + 1), y)
                                }
                            }
                        else
                            (0 until grid.width).forall { x =>
                                (0 until period).forall { y =>
                                    grid.get(x, y) == grid.get(x, y + period * (rep + 1))
                                }
                            }
                    }
                else false
            }
        end findPeriod

        val gridDominantColor = if colorCounts.isEmpty then 0 else colorCounts.maxBy(_._2)._1

        val gridLabels = List(
            (Label.RepeatsHorizontally, findPeriod("x").isDefined),
            (Label.RepeatsVertically, findPeriod("y").isDefined),
            (Label.SingleColor, colorCounts.size == 1),
            (Label.Sparse, allColors.count(_ == gridDominantColor) > allColors.size * 0.75),
            (Label.Dense, allColors.count(_ == gridDominantColor) < allColors.size * 0.25)
        ).collect { case (prop, true) => prop }

        val gridFormation = detectObjectGridFormation(objectInfos)

        Analysis(
            objects = objectInfos,
            backgroundColor = config.backgroundColor,
            width = grid.width,
            height = grid.height,
            colorHistogram = colorCounts,
            symmetries = gridSymmetries,
            labels = gridLabels,
            objectGridFormation = gridFormation
        )
    end init

    // Detect if objects form a regular grid pattern (NxM arrangement)
    private def detectObjectGridFormation(objects: List[ObjectInfo]): Option[(Int, Int)] =
        if objects.size < 4 then
            None // Need at least 4 objects to form a grid
        else
            // Get unique X and Y coordinates from object positions
            val uniqueXCoords = objects.map(_.x).distinct.sorted
            val uniqueYCoords = objects.map(_.y).distinct.sorted

            val rows = uniqueYCoords.size
            val cols = uniqueXCoords.size

            // Check if we have a proper grid formation:
            // - Number of objects should equal rows * cols
            // - Each (x, y) position in the grid should have exactly one object
            if objects.size == rows * cols then
                val gridPositions =
                    for
                        x <- uniqueXCoords
                        y <- uniqueYCoords
                    yield (x, y)

                val objectPositions = objects.map(obj => (obj.x, obj.y)).toSet

                if gridPositions.toSet == objectPositions then
                    Some((rows, cols))
                else
                    None
                end if
            else
                None
            end if
    end detectObjectGridFormation

    // Label a connected component using flood-fill
    private def labelComponent(
        grid: Grid,
        startX: Int,
        startY: Int,
        targetColor: Int,
        connectivity: Connectivity,
        backgroundColor: Int,
        visited: Set[(Int, Int)]
    ): (Set[(Int, Int)], List[(Int, Int, Int)]) =
        val neighbors = connectivity match
            case Connectivity.FourConnected =>
                List((0, 1), (1, 0), (0, -1), (-1, 0))
            case Connectivity.EightConnected =>
                List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

        def fill(
            toVisit: List[(Int, Int)],
            currentVisited: Set[(Int, Int)],
            pixels: List[(Int, Int, Int)]
        ): (Set[(Int, Int)], List[(Int, Int, Int)]) =
            toVisit match
                case Nil => (currentVisited, pixels)
                case (px, py) :: rest =>
                    if currentVisited.contains((px, py)) || !grid.isValid(px, py) then
                        fill(rest, currentVisited, pixels)
                    else
                        val color = grid.get(px, py)
                        if color != targetColor || color == backgroundColor then
                            fill(rest, currentVisited, pixels)
                        else
                            val newNeighbors = neighbors.map { (dx, dy) =>
                                (px + dx, py + dy)
                            }
                            fill(
                                newNeighbors ++ rest,
                                currentVisited + ((px, py)),
                                (px, py, color) +: pixels
                            )
                        end if

        fill(List((startX, startY)), visited, List.empty)
    end labelComponent

    // Analyze individual object properties
    private def analyzeObject(
        grid: Grid,
        objectId: Int,
        pixels: List[(Int, Int, Int)]
    ): ObjectInfo =
        // Geometric properties
        val pixelCount = pixels.length
        val minX       = pixels.map(_._1).min
        val maxX       = pixels.map(_._1).max
        val minY       = pixels.map(_._2).min
        val maxY       = pixels.map(_._2).max
        val objWidth   = maxX - minX + 1
        val objHeight  = maxY - minY + 1
        val centerX    = minX + objWidth / 2
        val centerY    = minY + objHeight / 2

        // Color analysis
        val colorHistogram = pixels.map(_._3)
            .groupBy(identity)
            .view.mapValues(_.size).toMap

        // Shape detection
        val shapes = detectShapes(pixels, minX, minY, objWidth, objHeight)

        // Symmetry detection
        val symmetries = detectSymmetries(pixels, minX, minY, objWidth, objHeight)

        // Position detection
        val positions = detectPositions(grid, minX, minY, maxX, maxY)

        // Density: ratio of pixels to bounding box
        val boundingBoxArea = objWidth * objHeight
        val density         = pixelCount.toDouble / boundingBoxArea

        ObjectInfo(
            objectId,
            pixelCount,
            centerX,
            centerY,
            objWidth,
            objHeight,
            colorHistogram,
            shapes,
            symmetries,
            positions,
            List.empty, // touching objects computed in second pass
            density
        )
    end analyzeObject

    // Compute touching objects based on pixel adjacency
    private def computeTouchingObjects(
        grid: Grid,
        obj: ObjectInfo,
        allObjects: List[ObjectInfo],
        connectivity: Connectivity
    ): List[Int] =
        // Calculate bounding box from center position
        val minX = obj.x - obj.width / 2
        val minY = obj.y - obj.height / 2

        // Get all pixels for this object
        val objPixels = (minY until minY + obj.height).flatMap { y =>
            (minX until minX + obj.width).flatMap { x =>
                if grid.get(x, y) != 0 && grid.isValid(x, y) then Some((x, y)) else None
            }
        }.toSet

        // Get adjacency offsets based on connectivity
        val offsets = connectivity match
            case Connectivity.FourConnected  => List((0, 1), (1, 0), (0, -1), (-1, 0))
            case Connectivity.EightConnected => List((0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1))

        // Find all adjacent pixels and their object IDs
        val touchingIds = objPixels.flatMap { (x, y) =>
            offsets.flatMap { (dx, dy) =>
                val nx = x + dx
                val ny = y + dy
                if grid.isValid(nx, ny) then
                    val neighborValue = grid.get(nx, ny)
                    if neighborValue != 0 then
                        // Find which object this pixel belongs to
                        allObjects.find { other =>
                            val otherMinX = other.x - other.width / 2
                            val otherMinY = other.y - other.height / 2
                            other.id != obj.id &&
                            nx >= otherMinX && nx < otherMinX + other.width &&
                            ny >= otherMinY && ny < otherMinY + other.height &&
                            grid.get(nx, ny) != 0
                        }.map(_.id)
                    else None
                    end if
                else None
                end if
            }
        }.toList.distinct

        touchingIds
    end computeTouchingObjects

    // Compute alignment positions based on shared coordinates with other objects
    private def computeAlignmentPositions(
        obj: ObjectInfo,
        allObjects: List[ObjectInfo]
    ): List[Position] =
        val otherObjects = allObjects.filter(_.id != obj.id)

        if otherObjects.isEmpty then List.empty
        else
            val alignedHorizontally = otherObjects.exists(other => other.y == obj.y)
            val alignedVertically   = otherObjects.exists(other => other.x == obj.x)

            val horizontal = if alignedHorizontally then List(Position.AlignedHorizontally) else List.empty
            val vertical   = if alignedVertically then List(Position.AlignedVertically) else List.empty

            horizontal ++ vertical
        end if
    end computeAlignmentPositions

    // Shape detection
    private def detectShapes(
        pixels: List[(Int, Int, Int)],
        minX: Int,
        minY: Int,
        objWidth: Int,
        objHeight: Int
    ): List[Shape] =
        val pixelSet        = pixels.map((x, y, _) => (x, y)).toSet
        val pixelCount      = pixels.length
        val boundingBoxArea = objWidth * objHeight
        val fillRatio       = pixelCount.toDouble / boundingBoxArea.toDouble

        val singlePixel = if pixelCount == 1 then List(Shape.SinglePixel) else List.empty

        val line = if objWidth == 1 || objHeight == 1 then List(Shape.Line) else List.empty

        val square = if objWidth == objHeight && fillRatio > 0.9 then List(Shape.Square) else List.empty

        val rectangle = if objWidth != objHeight && fillRatio > 0.9 then List(Shape.Rectangle) else List.empty

        val solid = if fillRatio == 1.0 then List(Shape.Solid) else List.empty

        val hollow = if fillRatio < 1.0 && fillRatio > 0.3 && objWidth > 2 && objHeight > 2 then
            val hasEdgePixels = pixelSet.exists { (x, y) =>
                x == minX || x == minX + objWidth - 1 ||
                y == minY || y == minY + objHeight - 1
            }
            val hasInteriorGaps = (minY + 1 until minY + objHeight - 1).exists { y =>
                (minX + 1 until minX + objWidth - 1).exists { x =>
                    !pixelSet.contains((x, y))
                }
            }
            if hasEdgePixels && hasInteriorGaps then List(Shape.Hollow) else List.empty
        else List.empty

        val cross = if objWidth >= 3 && objHeight >= 3 then
            val centerX = minX + objWidth / 2
            val centerY = minY + objHeight / 2
            if pixelSet.contains((centerX, centerY)) then
                val hasTop    = pixelSet.exists((x, y) => x == centerX && y < centerY)
                val hasBottom = pixelSet.exists((x, y) => x == centerX && y > centerY)
                val hasLeft   = pixelSet.exists((x, y) => x < centerX && y == centerY)
                val hasRight  = pixelSet.exists((x, y) => x > centerX && y == centerY)
                if hasTop && hasBottom && hasLeft && hasRight then List(Shape.Cross) else List.empty
            else List.empty
            end if
        else List.empty

        val lshape = if objWidth >= 2 && objHeight >= 2 && fillRatio < 0.7 then
            val hasBottomRow = (minX until minX + objWidth).count(x =>
                pixelSet.contains((x, minY + objHeight - 1))
            ) >= objWidth / 2
            val hasLeftCol = (minY until minY + objHeight).count(y =>
                pixelSet.contains((minX, y))
            ) >= objHeight / 2
            val hasTopRow = (minX until minX + objWidth).count(x =>
                pixelSet.contains((x, minY))
            ) >= objWidth / 2
            val hasRightCol = (minY until minY + objHeight).count(y =>
                pixelSet.contains((minX + objWidth - 1, y))
            ) >= objHeight / 2

            if (hasBottomRow && hasLeftCol) || (hasBottomRow && hasRightCol) ||
                (hasTopRow && hasLeftCol) || (hasTopRow && hasRightCol)
            then List(Shape.LShape)
            else List.empty
            end if
        else List.empty

        val tshape = if objWidth >= 3 && objHeight >= 2 then
            val hasTopRow = (minX until minX + objWidth).count(x =>
                pixelSet.contains((x, minY))
            ) >= objWidth * 0.7
            val centerX = minX + objWidth / 2
            val hasCenterCol = (minY until minY + objHeight).count(y =>
                pixelSet.contains((centerX, y))
            ) >= objHeight * 0.7
            if hasTopRow && hasCenterCol then List(Shape.TShape) else List.empty
        else List.empty

        singlePixel ++ line ++ square ++ rectangle ++ solid ++ hollow ++ cross ++ lshape ++ tshape
    end detectShapes

    // Symmetry detection
    private def detectSymmetries(
        pixels: List[(Int, Int, Int)],
        minX: Int,
        minY: Int,
        objWidth: Int,
        objHeight: Int
    ): List[Symmetry] =
        val pixelSet = pixels.map((x, y, _) => (x, y)).toSet

        // Vertical symmetry (mirror across vertical center)
        val vertical =
            if (minY until minY + objHeight).forall { y =>
                    (0 until objWidth / 2).forall { dx =>
                        val leftX  = minX + dx
                        val rightX = minX + objWidth - 1 - dx
                        pixelSet.contains((leftX, y)) == pixelSet.contains((rightX, y))
                    }
                }
            then List(Symmetry.Vertical)
            else List.empty

        // Horizontal symmetry (mirror across horizontal center)
        val horizontal =
            if (minX until minX + objWidth).forall { x =>
                    (0 until objHeight / 2).forall { dy =>
                        val topY    = minY + dy
                        val bottomY = minY + objHeight - 1 - dy
                        pixelSet.contains((x, topY)) == pixelSet.contains((x, bottomY))
                    }
                }
            then List(Symmetry.Horizontal)
            else List.empty

        // Diagonal symmetry (mirror across main diagonal)
        val diagonal = if objWidth == objHeight then
            if (0 until objWidth).forall { i =>
                    (0 until objHeight).forall { j =>
                        pixelSet.contains((minX + i, minY + j)) ==
                            pixelSet.contains((minX + j, minY + i))
                    }
                }
            then List(Symmetry.Diagonal)
            else List.empty
        else List.empty

        // Anti-diagonal symmetry
        val antiDiagonal = if objWidth == objHeight then
            if (0 until objWidth).forall { i =>
                    (0 until objHeight).forall { j =>
                        pixelSet.contains((minX + i, minY + j)) ==
                            pixelSet.contains((minX + objWidth - 1 - j, minY + objHeight - 1 - i))
                    }
                }
            then List(Symmetry.AntiDiagonal)
            else List.empty
        else List.empty

        vertical ++ horizontal ++ diagonal ++ antiDiagonal
    end detectSymmetries

    // Position detection
    private def detectPositions(
        grid: Grid,
        minX: Int,
        minY: Int,
        maxX: Int,
        maxY: Int
    ): List[Position] =
        val touchesTop    = minY == 0
        val touchesBottom = maxY == grid.height - 1
        val touchesLeft   = minX == 0
        val touchesRight  = maxX == grid.width - 1

        val top    = if touchesTop then List(Position.TouchesTop) else List.empty
        val bottom = if touchesBottom then List(Position.TouchesBottom) else List.empty
        val left   = if touchesLeft then List(Position.TouchesLeft) else List.empty
        val right  = if touchesRight then List(Position.TouchesRight) else List.empty

        val edge = if touchesTop || touchesBottom || touchesLeft || touchesRight then
            List(Position.TouchesEdge)
        else List.empty

        // Corner: touches two adjacent edges
        val corner =
            if (touchesTop && touchesLeft) || (touchesTop && touchesRight) ||
                (touchesBottom && touchesLeft) || (touchesBottom && touchesRight)
            then List(Position.Corner)
            else List.empty

        // Centered: bounding box center near grid center
        val centerX     = (minX + maxX) / 2.0
        val centerY     = (minY + maxY) / 2.0
        val gridCenterX = grid.width / 2.0
        val gridCenterY = grid.height / 2.0
        val threshold   = math.min(grid.width, grid.height) * 0.2

        val centered =
            if math.abs(centerX - gridCenterX) < threshold &&
                math.abs(centerY - gridCenterY) < threshold
            then List(Position.Centered)
            else List.empty

        top ++ bottom ++ left ++ right ++ edge ++ corner ++ centered
    end detectPositions

end Analysis

case class ObjectInfo(
    id: Int,
    pixelCount: Int,
    @doc("X coordinate of object center")
    x: Int,
    @doc("Y coordinate of object center")
    y: Int,
    width: Int,
    height: Int,
    colorHistogram: Map[Int, Int],
    shape: List[Shape],
    symmetry: List[Symmetry],
    position: List[Position],
    @doc("Object IDs of objects that touch this object (based on connectivity)")
    touchingObjectIds: List[Int],
    @doc("Ratio of actual pixels to bounding box area (1.0 = solid, <1.0 = sparse/incomplete)")
    density: Double
)
