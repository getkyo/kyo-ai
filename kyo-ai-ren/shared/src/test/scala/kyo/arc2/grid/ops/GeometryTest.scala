package kyo.arc2.grid.ops

import kyo.*
import kyo.arc2.{grid as _, *}
import kyo.arc2.TestUtils.*
import kyo.arc2.grid.*

class GeometryTest extends Test:

    "Geometry Operations" - {

        "Rotate" - {

            "90-degree rotations" - {

                "R90 clockwise rotation" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(3)(
                        7, 4, 1,
                        8, 5, 2,
                        9, 6, 3
                    )
                    val result = Geometry.Rotate(Rotation.R90)(input)
                    assert(result.cells == expected.cells)

                }

                "R180 half rotation" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(3)(
                        9, 8, 7,
                        6, 5, 4,
                        3, 2, 1
                    )
                    val result = Geometry.Rotate(Rotation.R180)(input)
                    assert(result.cells == expected.cells)

                }

                "R270 counter-clockwise" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(3)(
                        3, 6, 9,
                        2, 5, 8,
                        1, 4, 7
                    )
                    val result = Geometry.Rotate(Rotation.R270)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Rotation composition" - {

                "R90 four times is identity" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val r1     = Geometry.Rotate(Rotation.R90)(input)
                    val r2     = Geometry.Rotate(Rotation.R90)(r1)
                    val r3     = Geometry.Rotate(Rotation.R90)(r2)
                    val result = Geometry.Rotate(Rotation.R90)(r3)
                    assert(result.cells == input.cells)
                }

                "R180 twice is identity" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val r1     = Geometry.Rotate(Rotation.R180)(input)
                    val result = Geometry.Rotate(Rotation.R180)(r1)
                    assert(result.cells == input.cells)
                }

                "R90 + R270 is identity" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val r90    = Geometry.Rotate(Rotation.R90)(input)
                    val result = Geometry.Rotate(Rotation.R270)(r90)
                    // R90 followed by R270 should return to original
                    assert(result.cells == input.cells)
                }
            }

            "Pattern preservation" - {

                "maintains pixel count" in run {
                    val input = grid(4)(
                        1, 2, 3, 4,
                        5, 6, 7, 8,
                        9, 1, 2, 3,
                        4, 5, 6, 7
                    )
                    val result = Geometry.Rotate(Rotation.R90)(input)
                    assert(result.cells.flatten.size == input.cells.flatten.size)

                }

                "preserves color distribution" in run {
                    val input = grid(3)(
                        1, 1, 2,
                        2, 3, 3,
                        1, 2, 3
                    )
                    val result       = Geometry.Rotate(Rotation.R180)(input)
                    val inputColors  = input.cells.flatten.groupBy(identity).view.mapValues(_.size).toMap
                    val resultColors = result.cells.flatten.groupBy(identity).view.mapValues(_.size).toMap
                    assert(inputColors == resultColors)

                }
            }

            "Edge cases" - {

                "empty grid" in run {
                    val input  = Grid(List.empty)
                    val result = Geometry.Rotate(Rotation.R90)(input)
                    assert(result.cells == input.cells)

                }

                "single pixel" in run {
                    val input  = grid(1)(5)
                    val result = Geometry.Rotate(Rotation.R90)(input)
                    assert(result.cells == input.cells)

                }

                "square grid" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Rotate(Rotation.R90)(input)
                    // Square maintains dimensions after rotation
                    assert(result.width == input.width)
                    assert(result.height == input.height)

                }

                "rectangular grid" in run {
                    val input = grid(4)(
                        1, 2, 3, 4,
                        5, 6, 7, 8
                    )
                    val result = Geometry.Rotate(Rotation.R90)(input)
                    // Rectangle swaps dimensions after 90° rotation
                    assert(result.width == input.height)
                    assert(result.height == input.width)

                }
            }
        }

        "Flip" - {

            "Axis variants" - {

                "horizontal flip mirrors vertically" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(3)(
                        7, 8, 9,
                        4, 5, 6,
                        1, 2, 3
                    )
                    val result = Geometry.Flip(Axis.Horizontal)(input)
                    assert(result.cells == expected.cells)

                }

                "vertical flip mirrors horizontally" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(3)(
                        3, 2, 1,
                        6, 5, 4,
                        9, 8, 7
                    )
                    val result = Geometry.Flip(Axis.Vertical)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Flip composition" - {

                "flip twice is identity" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val flip1  = Geometry.Flip(Axis.Horizontal)(input)
                    val result = Geometry.Flip(Axis.Horizontal)(flip1)
                    assert(result.cells == input.cells)
                }

                "flip both axes equals R180" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val flipH   = Geometry.Flip(Axis.Horizontal)(input)
                    val flipped = Geometry.Flip(Axis.Vertical)(flipH)
                    val rotated = Geometry.Rotate(Rotation.R180)(input)
                    assert(flipped.cells == rotated.cells)
                }
            }

            "Symmetry interaction" - {

                "vertically symmetric pattern" in run {
                    val input = grid(3)(
                        1, 2, 1,
                        3, 4, 3,
                        1, 2, 1
                    )
                    val result = Geometry.Flip(Axis.Vertical)(input)
                    // Pattern is symmetric, so flip should equal original
                    assert(result.cells == input.cells)

                }

                "asymmetric pattern" in run {
                    val input = grid(3)(
                        1, 0, 0,
                        0, 1, 0,
                        0, 0, 1
                    )
                    val expected = grid(3)(
                        0, 0, 1,
                        0, 1, 0,
                        1, 0, 0
                    )
                    val result = Geometry.Flip(Axis.Horizontal)(input)
                    assert(result.cells == expected.cells)
                    assert(result.cells != input.cells)

                }
            }
        }

        "Transpose" - {

            "Basic transposition" - {

                "rows become columns" in run {
                    val input = grid(4)(
                        1, 2, 3, 4,
                        5, 6, 7, 8
                    )
                    val expected = grid(2)(
                        1, 5,
                        2, 6,
                        3, 7,
                        4, 8
                    )
                    val result = Geometry.Transpose()(input)
                    assert(result.cells == expected.cells)

                }

                "diagonal reflection" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(3)(
                        1, 4, 7,
                        2, 5, 8,
                        3, 6, 9
                    )
                    val result = Geometry.Transpose()(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Special cases" - {

                "square grid transpose" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Transpose()(input)
                    // Square maintains dimensions after transpose
                    assert(result.width == input.width)
                    assert(result.height == input.height)

                }

                "single row becomes column" in run {
                    val input = grid(4)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(1)(
                        1,
                        2,
                        3,
                        4
                    )
                    val result = Geometry.Transpose()(input)
                    assert(result.cells == expected.cells)
                    assert(result.width == 1)
                    assert(result.height == 4)

                }

                "empty grid" in run {
                    val input  = Grid(List.empty)
                    val result = Geometry.Transpose()(input)
                    assert(result.cells == input.cells)

                }
            }

            "Composition" - {

                "transpose twice is identity" in run {
                    val input = grid(4)(
                        1, 2, 3, 4,
                        5, 6, 7, 8
                    )
                    val t1     = Geometry.Transpose()(input)
                    val result = Geometry.Transpose()(t1)
                    assert(result.cells == input.cells)
                }

                "transpose + flip combinations" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val transposed    = Geometry.Transpose()(input)
                    val transposeFlip = Geometry.Flip(Axis.Vertical)(transposed)
                    val r90           = Geometry.Rotate(Rotation.R90)(input)
                    assert(transposeFlip.cells == r90.cells)
                }
            }
        }

        "Scale" - {

            "Uniform scaling" - {

                "2× scale doubles dimensions" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        1, 1, 2, 2,
                        1, 1, 2, 2,
                        3, 3, 4, 4,
                        3, 3, 4, 4
                    )
                    val result = Geometry.Scale(factorX = 2, factorY = 2)(input)
                    assert(result.cells == expected.cells)
                    assert(result.width == input.width * 2)
                    assert(result.height == input.height * 2)

                }

                "3× scale triples dimensions" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(6)(
                        1, 1, 1, 2, 2, 2,
                        1, 1, 1, 2, 2, 2,
                        1, 1, 1, 2, 2, 2,
                        3, 3, 3, 4, 4, 4,
                        3, 3, 3, 4, 4, 4,
                        3, 3, 3, 4, 4, 4
                    )
                    val result = Geometry.Scale(factorX = 3, factorY = 3)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Non-uniform scaling" - {

                "horizontal only (2×,1×)" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        1, 1, 2, 2,
                        3, 3, 4, 4
                    )
                    val result = Geometry.Scale(factorX = 2, factorY = 1)(input)
                    assert(result.cells == expected.cells)

                }

                "vertical only (1×,2×)" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(2)(
                        1, 2,
                        1, 2,
                        3, 4,
                        3, 4
                    )
                    val result = Geometry.Scale(factorX = 1, factorY = 2)(input)
                    assert(result.cells == expected.cells)

                }

                "different factors (3×,2×)" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(6)(
                        1, 1, 1, 2, 2, 2,
                        1, 1, 1, 2, 2, 2,
                        3, 3, 3, 4, 4, 4,
                        3, 3, 3, 4, 4, 4
                    )
                    val result = Geometry.Scale(factorX = 3, factorY = 2)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Pattern verification" - {

                "maintains pixel adjacency" in run {
                    val input = grid(3)(
                        1, 1, 0,
                        1, 1, 0,
                        0, 0, 0
                    )
                    val result = Geometry.Scale(factorX = 2, factorY = 2)(input)
                    // Adjacent 1s in input should remain adjacent in output
                    assert(result.cells(0)(0) == 1)
                    assert(result.cells(0)(1) == 1) // horizontal adjacency
                    assert(result.cells(1)(0) == 1) // vertical adjacency

                }

                "preserves shapes" in run {
                    val input = grid(3)(
                        0, 1, 0,
                        1, 1, 1,
                        0, 1, 0
                    )
                    val result = Geometry.Scale(factorX = 2, factorY = 2)(input)
                    // Cross shape should remain recognizable after scaling
                    // Center should be all 1s
                    assert(result.cells(2)(2) == 1)
                    assert(result.cells(2)(3) == 1)
                    assert(result.cells(3)(2) == 1)
                    assert(result.cells(3)(3) == 1)

                }
            }

            "Edge cases" - {

                "scale by 1 is identity" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Scale(factorX = 1, factorY = 1)(input)
                    assert(result.cells == input.cells)

                }

                "large scale factors" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val result = Geometry.Scale(factorX = 5, factorY = 5)(input)
                    assert(result.width == 10)
                    assert(result.height == 10)
                    assert(result.cells(0)(0) == 1)
                    assert(result.cells(4)(4) == 1)

                }
            }
        }

        "Crop" - {

            "Basic cropping" - {

                "crop center region" in run {
                    val input = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 1, 2, 3, 0,
                        0, 4, 5, 6, 0,
                        0, 7, 8, 9, 0,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Crop(x = 1, y = 1, width = 3, height = 3)(input)
                    assert(result.cells == expected.cells)

                }

                "crop from corner" in run {
                    val input = grid(4)(
                        1, 2, 3, 4,
                        5, 6, 7, 8,
                        9, 1, 2, 3,
                        4, 5, 6, 7
                    )
                    val expected = grid(2)(
                        1,
                        2,
                        5,
                        6
                    )
                    val result = Geometry.Crop(x = 0, y = 0, width = 2, height = 2)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Boundary cases" - {

                "crop entire grid" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Crop(x = 0, y = 0, width = 3, height = 3)(input)
                    assert(result.cells == input.cells)

                }

                "crop single pixel" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val expected = grid(1)(5)
                    val result   = Geometry.Crop(x = 1, y = 1, width = 1, height = 1)(input)
                    assert(result.cells == expected.cells)

                }

                "crop beyond bounds pads with zero" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    // Cropping beyond bounds pads with zeros to reach requested size
                    val expected = grid(5)(
                        9, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    val result = Geometry.Crop(x = 2, y = 2, width = 5, height = 5)(input)
                    assert(result.cells == expected.cells)
                    assert(result.width == 5)
                    assert(result.height == 5)

                }
            }

            "Pattern extraction" - {

                "extract specific object" in run {
                    val input = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 0, 1, 1, 0,
                        0, 0, 1, 1, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(2)(
                        1,
                        1,
                        1,
                        1
                    )
                    val result = Geometry.Crop(x = 2, y = 1, width = 2, height = 2)(input)
                    assert(result.cells == expected.cells)

                }

                "extract border region" in run {
                    val input = grid(4)(
                        9, 9, 9, 9,
                        9, 0, 0, 9,
                        9, 0, 0, 9,
                        9, 9, 9, 9
                    )
                    // Extract just the top border
                    val expected = grid(4)(
                        9,
                        9,
                        9,
                        9
                    )
                    val result = Geometry.Crop(x = 0, y = 0, width = 4, height = 1)(input)
                    assert(result.cells == expected.cells)

                }
            }
        }

        "Pad" - {

            "Uniform padding" - {

                "pad all sides equally" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        0, 0, 0, 0,
                        0, 1, 2, 0,
                        0, 3, 4, 0,
                        0, 0, 0, 0
                    )
                    val result = Geometry.Pad(top = 1, bottom = 1, left = 1, right = 1, fillColor = 0)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Asymmetric padding" - {

                "pad only top" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(2)(
                        0, 0,
                        1, 2,
                        3, 4
                    )
                    val result = Geometry.Pad(top = 1, bottom = 0, left = 0, right = 0, fillColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "pad left and right" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        0, 1, 2, 0,
                        0, 3, 4, 0
                    )
                    val result = Geometry.Pad(top = 0, bottom = 0, left = 1, right = 1, fillColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "different amounts per side" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(5)(
                        9, 9, 9, 9, 9,
                        9, 9, 9, 9, 9,
                        9, 9, 1, 2, 9,
                        9, 9, 3, 4, 9,
                        9, 9, 9, 9, 9
                    )
                    val result = Geometry.Pad(top = 2, bottom = 1, left = 2, right = 1, fillColor = 9)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Fill color variations" - {

                "background fill (0)" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(3)(
                        0, 0, 0,
                        0, 1, 2,
                        0, 3, 4
                    )
                    val result = Geometry.Pad(top = 1, bottom = 0, left = 1, right = 0, fillColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "visible border (1)" in run {
                    val input = grid(2)(
                        0,
                        0,
                        0,
                        0
                    )
                    val expected = grid(4)(
                        1, 1, 1, 1,
                        1, 0, 0, 1,
                        1, 0, 0, 1,
                        1, 1, 1, 1
                    )
                    val result = Geometry.Pad(top = 1, bottom = 1, left = 1, right = 1, fillColor = 1)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Edge cases" - {

                "zero padding is identity" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Pad(top = 0, bottom = 0, left = 0, right = 0, fillColor = 0)(input)
                    assert(result.cells == input.cells)

                }

                "large padding" in run {
                    val input  = grid(1)(5)
                    val result = Geometry.Pad(top = 5, bottom = 5, left = 5, right = 5, fillColor = 0)(input)
                    assert(result.width == 11)
                    assert(result.height == 11)
                    assert(result.cells(5)(5) == 5) // Center pixel
                    assert(result.cells(0)(0) == 0) // Corner padding

                }
            }
        }

        "Resize" - {

            "Enlarging" - {

                "resize larger with TopLeft anchor" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        1, 2, 0, 0,
                        3, 4, 0, 0,
                        0, 0, 0, 0,
                        0, 0, 0, 0
                    )
                    val result = Geometry.Resize(newWidth = 4, newHeight = 4, fillColor = 0, anchor = Anchor.TopLeft)(input)
                    assert(result.cells == expected.cells)

                }

                "resize larger with Center anchor" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        0, 0, 0, 0,
                        0, 1, 2, 0,
                        0, 3, 4, 0,
                        0, 0, 0, 0
                    )
                    val result = Geometry.Resize(newWidth = 4, newHeight = 4, fillColor = 0, anchor = Anchor.Center)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Shrinking" - {

                "resize smaller with TopLeft anchor" in run {
                    val input = grid(4)(
                        1, 2, 3, 4,
                        5, 6, 7, 8,
                        9, 1, 2, 3,
                        4, 5, 6, 7
                    )
                    val expected = grid(2)(
                        1,
                        2,
                        5,
                        6
                    )
                    val result = Geometry.Resize(newWidth = 2, newHeight = 2, fillColor = 0, anchor = Anchor.TopLeft)(input)
                    assert(result.cells == expected.cells)

                }

                "resize smaller with Center anchor" in run {
                    val input = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 1, 2, 3, 0,
                        0, 4, 5, 6, 0,
                        0, 7, 8, 9, 0,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Resize(newWidth = 3, newHeight = 3, fillColor = 0, anchor = Anchor.Center)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Aspect ratio changes" - {

                "resize to wider" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(4)(
                        1, 2, 0, 0,
                        3, 4, 0, 0
                    )
                    val result = Geometry.Resize(newWidth = 4, newHeight = 2, fillColor = 0, anchor = Anchor.TopLeft)(input)
                    assert(result.cells == expected.cells)

                }

                "resize to taller" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val expected = grid(2)(
                        1, 2,
                        3, 4,
                        0, 0,
                        0, 0
                    )
                    val result = Geometry.Resize(newWidth = 2, newHeight = 4, fillColor = 0, anchor = Anchor.TopLeft)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Edge cases" - {

                "resize to same dimensions" in run {
                    val input = grid(3)(
                        1, 2, 3,
                        4, 5, 6,
                        7, 8, 9
                    )
                    val result = Geometry.Resize(newWidth = 3, newHeight = 3, fillColor = 0, anchor = Anchor.TopLeft)(input)
                    assert(result.cells == input.cells)

                }

                "resize to larger then smaller" in run {
                    val input = grid(2)(
                        1,
                        2,
                        3,
                        4
                    )
                    val larger = Geometry.Resize(newWidth = 4, newHeight = 4, fillColor = 0, anchor = Anchor.TopLeft)(input)
                    val result = Geometry.Resize(newWidth = 2, newHeight = 2, fillColor = 0, anchor = Anchor.TopLeft)(larger)
                    assert(result.cells == input.cells)
                }
            }
        }

        "Tile" - {

            "tile 2x2 grid horizontally" in run {
                val input = grid(2)(
                    1,
                    2,
                    3,
                    4
                )
                val expected = grid(4)(
                    1, 2, 1, 2,
                    3, 4, 3, 4
                )
                val result = Geometry.Tile(timesX = 2, timesY = 1)(input)
                assert(result.cells == expected.cells)

            }

            "tile 2x2 grid vertically" in run {
                val input = grid(2)(
                    1,
                    2,
                    3,
                    4
                )
                val expected = grid(2)(
                    1, 2,
                    3, 4,
                    1, 2,
                    3, 4
                )
                val result = Geometry.Tile(timesX = 1, timesY = 2)(input)
                assert(result.cells == expected.cells)

            }

            "tile 3x3 grid in 3x3 pattern" in run {
                val input = grid(3)(
                    1, 2, 3,
                    4, 5, 6,
                    7, 8, 9
                )
                val expected = grid(9)(
                    1, 2, 3, 1, 2, 3, 1, 2, 3,
                    4, 5, 6, 4, 5, 6, 4, 5, 6,
                    7, 8, 9, 7, 8, 9, 7, 8, 9,
                    1, 2, 3, 1, 2, 3, 1, 2, 3,
                    4, 5, 6, 4, 5, 6, 4, 5, 6,
                    7, 8, 9, 7, 8, 9, 7, 8, 9,
                    1, 2, 3, 1, 2, 3, 1, 2, 3,
                    4, 5, 6, 4, 5, 6, 4, 5, 6,
                    7, 8, 9, 7, 8, 9, 7, 8, 9
                )
                val result = Geometry.Tile(timesX = 3, timesY = 3)(input)
                assert(result.cells == expected.cells)

            }

            "tile 1x1 creates larger grid of same value" in run {
                val input = grid(1)(5)
                val expected = grid(3)(
                    5, 5, 5,
                    5, 5, 5,
                    5, 5, 5
                )
                val result = Geometry.Tile(timesX = 3, timesY = 3)(input)
                assert(result.cells == expected.cells)

            }
        }
    }
end GeometryTest
