// package kyo.arc2.grid.ops

// import kyo.*
// import kyo.arc2.{grid as _, *}
// import kyo.arc2.TestUtils.*
// import kyo.arc2.grid.*

// class ControlTest extends Test:

//     "Control Operations" - {

//         "Sequence" - {

//             "Basic sequencing" - {

//                 "empty operation list returns input unchanged" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Sequence(List.empty)(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "single operation applies normally" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val expected = grid(3)(
//                         7, 4, 1,
//                         8, 5, 2,
//                         9, 6, 3
//                     )
//                     val result = Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90)
//                     ))(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == expected.cells)
//                 }

//                 "two operations apply sequentially" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     // Rotate 90 then flip horizontal
//                     val expected = grid(3)(
//                         9, 6, 3,
//                         8, 5, 2,
//                         7, 4, 1
//                     )
//                     val result = Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     ))(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == expected.cells)
//                 }

//                 "three operations chain correctly" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     // Rotate 90, flip horizontal, rotate 90 again
//                     val step1 = Geometry.Rotate(Rotation.R90)(input)
//                     val step2 = Geometry.Flip(Axis.Horizontal)(step1)
//                     val step3 = Geometry.Rotate(Rotation.R90)(step2)

//                     val result = Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal),
//                         Geometry.Rotate(Rotation.R90)
//                     ))(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == step3.cells)
//                 }
//             }

//             "Pipeline verification" - {

//                 "output of first becomes input to second" in run {
//                     val input = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     // Scale 2x then crop back to 2x2
//                     val result = Control.Sequence(List(
//                         Geometry.Scale(factorX = 2, factorY = 2),
//                         Geometry.Crop(x = 0, y = 0, width = 2, height = 2)
//                     ))(List(input))
//                     // After scaling and cropping, should get top-left quadrant of scaled grid
//                     val expected = grid(2)(
//                         1,
//                         1,
//                         1,
//                         1
//                     )
//                     assert(result.size == 1)
//                     assert(result.head.cells == expected.cells)
//                 }

//                 "transformations accumulate" in run {
//                     val input = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     // Transpose twice should be identity
//                     val result = Control.Sequence(List(
//                         Geometry.Transpose(),
//                         Geometry.Transpose()
//                     ))(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }
//             }

//             "Edge cases" - {

//                 "sequence with empty grid" in run {
//                     val input = Grid(List.empty)
//                     val result = Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     ))(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "sequence with dimension-changing operations" in run {
//                     val input = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     // Scale then pad
//                     val result = Control.Sequence(List(
//                         Geometry.Scale(factorX = 2, factorY = 2),
//                         Geometry.Pad(top = 1, bottom = 1, left = 1, right = 1, fillColor = 0)
//                     ))(List(input))
//                     // Final should be 6x6 (4x4 scaled, plus 1 padding on each side)
//                     assert(result.size == 1)
//                     assert(result.head.width == 6)
//                     assert(result.head.height == 6)
//                 }
//             }

//             "Multiple grids" - {

//                 "sequence applies to all input grids" in run {
//                     val input1 = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     val input2 = grid(2)(
//                         5,
//                         6,
//                         7,
//                         8
//                     )
//                     val result = Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90)
//                     ))(List(input1, input2))

//                     assert(result.size == 2)
//                     assert(result(0).cells == Geometry.Rotate(Rotation.R90)(input1).cells)
//                     assert(result(1).cells == Geometry.Rotate(Rotation.R90)(input2).cells)
//                 }
//             }
//         }

//         "Parallel" - {

//             "Basic parallel" - {

//                 "empty operation list returns empty result" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Parallel(List.empty)(List(input))
//                     assert(result.isEmpty)
//                 }

//                 "single operation returns single result" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90)
//                     ))(List(input))
//                     assert(result.size == 1)
//                     assert(result(0).cells == Geometry.Rotate(Rotation.R90)(input).cells)
//                 }

//                 "two operations produce two results" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     ))(List(input))
//                     assert(result.size == 2)
//                     assert(result(0).cells == Geometry.Rotate(Rotation.R90)(input).cells)
//                     assert(result(1).cells == Geometry.Flip(Axis.Horizontal)(input).cells)
//                 }

//                 "three operations produce three results" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Rotate(Rotation.R180),
//                         Geometry.Rotate(Rotation.R270)
//                     ))(List(input))
//                     assert(result.size == 3)
//                     assert(result(0).cells == Geometry.Rotate(Rotation.R90)(input).cells)
//                     assert(result(1).cells == Geometry.Rotate(Rotation.R180)(input).cells)
//                     assert(result(2).cells == Geometry.Rotate(Rotation.R270)(input).cells)
//                 }
//             }

//             "Independence verification" - {

//                 "operations don't affect each other" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     // Each operation sees the original input
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Vertical)
//                     ))(List(input))

//                     // Verify each result is independent transformation of original
//                     val expectedRotate = Geometry.Rotate(Rotation.R90)(input)
//                     val expectedFlip   = Geometry.Flip(Axis.Vertical)(input)

//                     assert(result(0).cells == expectedRotate.cells)
//                     assert(result(1).cells == expectedFlip.cells)
//                     // Results should be different from each other
//                     assert(result(0).cells != result(1).cells)
//                 }

//                 "order of operations doesn't matter for result content" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result1 = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     ))(List(input))
//                     val result2 = Control.Parallel(List(
//                         Geometry.Flip(Axis.Horizontal),
//                         Geometry.Rotate(Rotation.R90)
//                     ))(List(input))

//                     // Results should contain same transformations but in different order
//                     assert(result1.size == result2.size)
//                     assert(result1(0).cells == result2(1).cells)
//                     assert(result1(1).cells == result2(0).cells)
//                 }
//             }

//             "Multiple input grids" - {

//                 "each operation processes all inputs" in run {
//                     val input1 = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     val input2 = grid(2)(
//                         5,
//                         6,
//                         7,
//                         8
//                     )
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     ))(List(input1, input2))

//                     // 2 operations × 2 inputs = 4 results
//                     assert(result.size == 4)

//                     // First operation (R90) applied to both inputs
//                     assert(result(0).cells == Geometry.Rotate(Rotation.R90)(input1).cells)
//                     assert(result(1).cells == Geometry.Rotate(Rotation.R90)(input2).cells)

//                     // Second operation (Flip) applied to both inputs
//                     assert(result(2).cells == Geometry.Flip(Axis.Horizontal)(input1).cells)
//                     assert(result(3).cells == Geometry.Flip(Axis.Horizontal)(input2).cells)
//                 }
//             }

//             "Edge cases" - {

//                 "parallel with empty grid" in run {
//                     val input = Grid(List.empty)
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     ))(List(input))
//                     assert(result.size == 2)
//                     assert(result(0).cells.isEmpty)
//                     assert(result(1).cells.isEmpty)
//                 }

//                 "parallel generates all variations" in run {
//                     val input = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     // Generate all 3 rotations in parallel
//                     val result = Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Rotate(Rotation.R180),
//                         Geometry.Rotate(Rotation.R270)
//                     ))(List(input))
//                     assert(result.size == 3)
//                 }
//             }
//         }

//         "Repeat" - {

//             "Basic repetition" - {

//                 "repeat 0 times returns input unchanged" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 0
//                     )(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "repeat 1 time applies operation once" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 1
//                     )(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == Geometry.Rotate(Rotation.R90)(input).cells)
//                 }

//                 "repeat 2 times applies operation twice" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 2
//                     )(List(input))
//                     // Two R90 rotations = R180
//                     assert(result.size == 1)
//                     assert(result.head.cells == Geometry.Rotate(Rotation.R180)(input).cells)
//                 }

//                 "repeat 3 times applies operation three times" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 3
//                     )(List(input))
//                     // Three R90 rotations = R270
//                     assert(result.size == 1)
//                     assert(result.head.cells == Geometry.Rotate(Rotation.R270)(input).cells)
//                 }

//                 "repeat 4 times with R90 is identity" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 4
//                     )(List(input))
//                     // Four R90 rotations return to original
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }
//             }

//             "Accumulation verification" - {

//                 "transformations accumulate sequentially" in run {
//                     val input = grid(3)(
//                         1, 0, 0,
//                         0, 0, 0,
//                         0, 0, 0
//                     )
//                     // Pad 1 pixel on all sides, repeated 3 times
//                     val result = Control.Repeat(
//                         Geometry.Pad(top = 1, bottom = 1, left = 1, right = 1, fillColor = 0),
//                         times = 3
//                     )(List(input))
//                     // Should add 2 pixels per iteration (1 on each side)
//                     // Original: 3x3, after 1: 5x5, after 2: 7x7, after 3: 9x9
//                     assert(result.size == 1)
//                     assert(result.head.width == 9)
//                     assert(result.head.height == 9)
//                 }

//                 "idempotent operation repeated is stable" in run {
//                     val input = grid(3)(
//                         1, 2, 1,
//                         3, 4, 3,
//                         1, 2, 1
//                     )
//                     // Flipping a symmetric pattern multiple times
//                     val result = Control.Repeat(
//                         Geometry.Flip(Axis.Vertical),
//                         times = 10
//                     )(List(input))
//                     // Even number of flips returns to original
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }
//             }

//             "Edge cases" - {

//                 "repeat with empty grid" in run {
//                     val input = Grid(List.empty)
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 5
//                     )(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells.isEmpty)
//                 }

//                 "repeat large number of times" in run {
//                     val input = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     // R180 repeated even number of times is identity
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R180),
//                         times = 100
//                     )(List(input))
//                     assert(result.size == 1)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "repeat with dimension-changing operation" in run {
//                     val input = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     // Scale by 2, repeated 3 times
//                     val result = Control.Repeat(
//                         Geometry.Scale(factorX = 2, factorY = 2),
//                         times = 3
//                     )(List(input))
//                     // 2x2 -> 4x4 -> 8x8 -> 16x16
//                     assert(result.size == 1)
//                     assert(result.head.width == 16)
//                     assert(result.head.height == 16)
//                 }
//             }

//             "Multiple grids" - {

//                 "repeat applies to all input grids" in run {
//                     val input1 = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     val input2 = grid(2)(
//                         5,
//                         6,
//                         7,
//                         8
//                     )
//                     val result = Control.Repeat(
//                         Geometry.Rotate(Rotation.R90),
//                         times = 2
//                     )(List(input1, input2))

//                     assert(result.size == 2)
//                     assert(result(0).cells == Geometry.Rotate(Rotation.R180)(input1).cells)
//                     assert(result(1).cells == Geometry.Rotate(Rotation.R180)(input2).cells)
//                 }
//             }
//         }

//         "AnalysisParams" - {

//             "Connectivity changes" - {

//                 "change to FourConnected" in run {
//                     val input = Grid(
//                         List(
//                             List(1, 0, 1),
//                             List(0, 1, 0),
//                             List(1, 0, 1)
//                         )
//                     )
//                     val result = Control.AnalysisParams(
//                         connectivity = Some(Connectivity.FourConnected)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.connectivity == Connectivity.FourConnected)
//                     assert(result.head.cells == input.cells) // Cells unchanged
//                 }

//                 "change to EightConnected" in run {
//                     val input = Grid(
//                         List(
//                             List(1, 0, 1),
//                             List(0, 1, 0),
//                             List(1, 0, 1)
//                         )
//                     )
//                     val result = Control.AnalysisParams(
//                         connectivity = Some(Connectivity.EightConnected)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.connectivity == Connectivity.EightConnected)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "None preserves original connectivity" in run {
//                     val input = Grid(
//                         List(
//                             List(1, 0, 1),
//                             List(0, 1, 0),
//                             List(1, 0, 1)
//                         ),
//                         Analysis.Config(connectivity = Connectivity.FourConnected)
//                     )
//                     val result = Control.AnalysisParams(
//                         connectivity = None
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.connectivity == Connectivity.FourConnected)
//                 }
//             }

//             "Background color changes" - {

//                 "change background to 1" in run {
//                     val input = grid(3)(
//                         0, 1, 0,
//                         1, 2, 1,
//                         0, 1, 0
//                     )
//                     val result = Control.AnalysisParams(
//                         backgroundColor = Some(1)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.backgroundColor == 1)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "change background to 9" in run {
//                     val input = grid(3)(
//                         0, 1, 0,
//                         1, 2, 1,
//                         0, 1, 0
//                     )
//                     val result = Control.AnalysisParams(
//                         backgroundColor = Some(9)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.backgroundColor == 9)
//                 }

//                 "None preserves original background" in run {
//                     val input = Grid(
//                         List(
//                             List(0, 1, 0),
//                             List(1, 2, 1),
//                             List(0, 1, 0)
//                         ),
//                         Analysis.Config(backgroundColor = 5)
//                     )
//                     val result = Control.AnalysisParams(
//                         backgroundColor = None
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.backgroundColor == 5)
//                 }
//             }

//             "Minimum size changes" - {

//                 "change minSize to 5" in run {
//                     val input = grid(3)(
//                         1, 1, 1,
//                         1, 1, 1,
//                         1, 1, 1
//                     )
//                     val result = Control.AnalysisParams(
//                         minSize = Some(5)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.minSize == 5)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "change minSize to 1" in run {
//                     val input = grid(3)(
//                         1, 0, 1,
//                         0, 1, 0,
//                         1, 0, 1
//                     )
//                     val result = Control.AnalysisParams(
//                         minSize = Some(1)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.minSize == 1)
//                 }

//                 "None preserves original minSize" in run {
//                     val input = Grid(
//                         List(
//                             List(1, 0, 1),
//                             List(0, 1, 0),
//                             List(1, 0, 1)
//                         ),
//                         Analysis.Config(minSize = 10)
//                     )
//                     val result = Control.AnalysisParams(
//                         minSize = None
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.minSize == 10)
//                 }
//             }

//             "Multiple parameter changes" - {

//                 "change all parameters" in run {
//                     val input = grid(3)(
//                         1, 0, 1,
//                         0, 1, 0,
//                         1, 0, 1
//                     )
//                     val result = Control.AnalysisParams(
//                         connectivity = Some(Connectivity.EightConnected),
//                         backgroundColor = Some(2),
//                         minSize = Some(3)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.connectivity == Connectivity.EightConnected)
//                     assert(result.head.config.backgroundColor == 2)
//                     assert(result.head.config.minSize == 3)
//                     assert(result.head.cells == input.cells)
//                 }

//                 "change some parameters, preserve others" in run {
//                     val input = Grid(
//                         List(
//                             List(1, 0, 1),
//                             List(0, 1, 0),
//                             List(1, 0, 1)
//                         ),
//                         Analysis.Config(
//                             connectivity = Connectivity.FourConnected,
//                             backgroundColor = 5,
//                             minSize = 10
//                         )
//                     )
//                     val result = Control.AnalysisParams(
//                         connectivity = Some(Connectivity.EightConnected),
//                         backgroundColor = None,
//                         minSize = Some(2)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.connectivity == Connectivity.EightConnected)
//                     assert(result.head.config.backgroundColor == 5) // Preserved
//                     assert(result.head.config.minSize == 2)
//                 }

//                 "all None preserves all parameters" in run {
//                     val input = Grid(
//                         List(
//                             List(1, 0, 1),
//                             List(0, 1, 0),
//                             List(1, 0, 1)
//                         ),
//                         Analysis.Config(
//                             connectivity = Connectivity.FourConnected,
//                             backgroundColor = 7,
//                             minSize = 15
//                         )
//                     )
//                     val result = Control.AnalysisParams(
//                         connectivity = None,
//                         backgroundColor = None,
//                         minSize = None
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config == input.config)
//                     assert(result.head.cells == input.cells)
//                 }
//             }

//             "Multiple grids" - {

//                 "applies to all input grids" in run {
//                     val input1 = grid(2)(
//                         1,
//                         2,
//                         3,
//                         4
//                     )
//                     val input2 = grid(2)(
//                         5,
//                         6,
//                         7,
//                         8
//                     )
//                     val result = Control.AnalysisParams(
//                         backgroundColor = Some(5),
//                         minSize = Some(2)
//                     )(List(input1, input2))

//                     assert(result.size == 2)
//                     assert(result(0).config.backgroundColor == 5)
//                     assert(result(0).config.minSize == 2)
//                     assert(result(1).config.backgroundColor == 5)
//                     assert(result(1).config.minSize == 2)
//                 }
//             }

//             "Edge cases" - {

//                 "empty grid preserves config" in run {
//                     val input = Grid(List.empty)
//                     val result = Control.AnalysisParams(
//                         connectivity = Some(Connectivity.EightConnected),
//                         backgroundColor = Some(3),
//                         minSize = Some(5)
//                     )(List(input))

//                     assert(result.size == 1)
//                     assert(result.head.config.connectivity == Connectivity.EightConnected)
//                     assert(result.head.config.backgroundColor == 3)
//                     assert(result.head.config.minSize == 5)
//                     assert(result.head.cells.isEmpty)
//                 }
//             }
//         }

//         "Composition of control operations" - {

//             "Sequence of Parallels" in run {
//                 val input = grid(2)(
//                     1,
//                     2,
//                     3,
//                     4
//                 )
//                 // First parallel generates 2 outputs, second parallel operates on both
//                 val result = Control.Sequence(List(
//                     Control.Parallel(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     )),
//                     Control.Parallel(List(
//                         Geometry.Transpose()
//                     ))
//                 ))(List(input))

//                 // First parallel: 2 results
//                 // Second parallel: 1 operation on 2 inputs = 2 results
//                 assert(result.size == 2)
//             }

//             "Parallel of Sequences" in run {
//                 val input = grid(2)(
//                     1,
//                     2,
//                     3,
//                     4
//                 )
//                 // Two different sequence pipelines applied to same input
//                 val result = Control.Parallel(List(
//                     Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     )),
//                     Control.Sequence(List(
//                         Geometry.Flip(Axis.Vertical),
//                         Geometry.Rotate(Rotation.R90)
//                     ))
//                 ))(List(input))

//                 assert(result.size == 2)
//             }

//             "Repeat of Sequence" in run {
//                 val input = grid(2)(
//                     1,
//                     2,
//                     3,
//                     4
//                 )
//                 // Repeat a sequence of operations
//                 val result = Control.Repeat(
//                     Control.Sequence(List(
//                         Geometry.Rotate(Rotation.R90),
//                         Geometry.Flip(Axis.Horizontal)
//                     )),
//                     times = 2
//                 )(List(input))

//                 assert(result.size == 1)
//                 assert(result.head.cells.nonEmpty)
//             }

//             "AnalysisParams then operations" in run {
//                 val input = grid(3)(
//                     1, 0, 1,
//                     0, 1, 0,
//                     1, 0, 1
//                 )
//                 // Change analysis params, then apply transformation
//                 val result = Control.Sequence(List(
//                     Control.AnalysisParams(
//                         connectivity = Some(Connectivity.EightConnected),
//                         backgroundColor = Some(1)
//                     ),
//                     Geometry.Rotate(Rotation.R90)
//                 ))(List(input))

//                 // Config should be preserved through transformation
//                 assert(result.size == 1)
//                 assert(result.head.config.connectivity == Connectivity.EightConnected)
//                 assert(result.head.config.backgroundColor == 1)
//             }
//         }
//     }
// end ControlTest
