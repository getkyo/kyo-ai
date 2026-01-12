// package kyo.arc2.grid.ops

// import kyo.*
// import kyo.arc2.{grid as _, *}
// import kyo.arc2.TestUtils.*
// import kyo.arc2.grid.*

// class ColorTest extends Test:

//     "Color Operations" - {

//         "ReplaceColor" - {

//             "Basic replacement" - {

//                 "single color mapping" in run {
//                     val input = grid(3)(
//                         1, 1, 0,
//                         1, 0, 1,
//                         0, 1, 1
//                     )
//                     val expected = grid(3)(
//                         2, 2, 0,
//                         2, 0, 2,
//                         0, 2, 2
//                     )
//                     val result = Color.ReplaceColors(Map(1 -> 2))(input)
//                     assert(result.cells == expected.cells)
//                 }

//                 "multiple color mappings" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         2, 3, 1,
//                         3, 1, 2
//                     )
//                     val expected = grid(3)(
//                         9, 8, 7,
//                         8, 7, 9,
//                         7, 9, 8
//                     )
//                     val result = Color.ReplaceColors(Map(1 -> 9, 2 -> 8, 3 -> 7))(input)
//                     assert(result.cells == expected.cells)
//                 }
//             }

//             "Unmapped colors" - {

//                 "unmapped colors pass through" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         2, 3, 1,
//                         3, 1, 2
//                     )
//                     val expected = grid(3)(
//                         9, 2, 3,
//                         2, 3, 9,
//                         3, 9, 2
//                     )
//                     val result = Color.ReplaceColors(Map(1 -> 9))(input)
//                     assert(result.cells == expected.cells)
//                 }
//             }

//             "Identity and swaps" - {

//                 "identity mapping" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         2, 3, 1,
//                         3, 1, 2
//                     )
//                     val result = Color.ReplaceColors(Map(1 -> 1, 2 -> 2, 3 -> 3))(input)
//                     assert(result.cells == input.cells)

//                 }

//                 "color swap" in run {
//                     val input = grid(3)(
//                         1, 2, 1,
//                         2, 1, 2,
//                         1, 2, 1
//                     )
//                     val expected = grid(3)(
//                         2, 1, 2,
//                         1, 2, 1,
//                         2, 1, 2
//                     )
//                     val result = Color.ReplaceColors(Map(1 -> 2, 2 -> 1))(input)
//                     assert(result.cells == expected.cells)

//                 }
//             }

//             "Edge cases" - {

//                 "empty mapping" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         2, 3, 1,
//                         3, 1, 2
//                     )
//                     val result = Color.ReplaceColors(Map.empty)(input)
//                     assert(result.cells == input.cells)

//                 }

//                 "map to same color" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         2, 3, 1,
//                         3, 1, 2
//                     )
//                     val expected = grid(3)(
//                         5, 5, 3,
//                         5, 3, 5,
//                         3, 5, 5
//                     )
//                     val result = Color.ReplaceColors(Map(1 -> 5, 2 -> 5))(input)
//                     assert(result.cells == expected.cells)

//                 }
//             }
//         }

//         "NormalizeColors" - {

//             "Basic normalization" - {

//                 "renumber to 1,2,3" in run {
//                     val input = grid(3)(
//                         5, 8, 0,
//                         8, 0, 5,
//                         0, 5, 8
//                     )
//                     val expected = grid(3)(
//                         1, 2, 0,
//                         2, 0, 1,
//                         0, 1, 2
//                     )
//                     val result = Color.NormalizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "background excluded" in run {
//                     val input = grid(3)(
//                         3, 3, 0,
//                         3, 0, 3,
//                         0, 0, 0
//                     )
//                     val expected = grid(3)(
//                         1, 1, 0,
//                         1, 0, 1,
//                         0, 0, 0
//                     )
//                     val result = Color.NormalizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)
//                     assert(!result.cells.flatten.contains(3))

//                 }
//             }

//             "Order preservation" - {

//                 "sorted order deterministic" in run {
//                     val input = grid(3)(
//                         7, 3, 9,
//                         3, 9, 7,
//                         9, 7, 3
//                     )
//                     val expected = grid(3)(
//                         2, 1, 3,
//                         1, 3, 2,
//                         3, 2, 1
//                     )
//                     // Colors sorted: 3->1, 7->2, 9->3
//                     val result = Color.NormalizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "relative order by value" in run {
//                     val input1 = grid(2)(
//                         2,
//                         5,
//                         5,
//                         2
//                     )
//                     val input2 = grid(2)(
//                         5,
//                         2,
//                         2,
//                         5
//                     )
//                     val expected1 = grid(2)(
//                         1,
//                         2,
//                         2,
//                         1
//                     )
//                     val expected2 = grid(2)(
//                         2,
//                         1,
//                         1,
//                         2
//                     )
//                     // Both should normalize the same way: 2->1, 5->2
//                     val result1 = Color.NormalizeColors(backgroundColor = 0)(input1)
//                     val result2 = Color.NormalizeColors(backgroundColor = 0)(input2)
//                     assert(result1.cells == expected1.cells)
//                     assert(result2.cells == expected2.cells)
//                 }
//             }

//             "Edge cases" - {

//                 "single color" in run {
//                     val input = grid(3)(
//                         5, 5, 0,
//                         5, 0, 5,
//                         0, 5, 5
//                     )
//                     val expected = grid(3)(
//                         1, 1, 0,
//                         1, 0, 1,
//                         0, 1, 1
//                     )
//                     val result = Color.NormalizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "already normalized" in run {
//                     val input = grid(3)(
//                         1, 2, 0,
//                         2, 0, 1,
//                         0, 1, 2
//                     )
//                     val result = Color.NormalizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == input.cells)

//                 }

//                 "non-zero background" in run {
//                     val input = grid(3)(
//                         5, 8, 9,
//                         8, 9, 5,
//                         9, 5, 8
//                     )
//                     val expected = grid(3)(
//                         1, 2, 9,
//                         2, 9, 1,
//                         9, 1, 2
//                     )
//                     // Background is 9, so normalize 5->1, 8->2
//                     val result = Color.NormalizeColors(backgroundColor = 9)(input)
//                     assert(result.cells == expected.cells)

//                 }
//             }
//         }

//         "InvertColors" - {

//             "Basic inversion" - {

//                 "invert with maxColor=9" in run {
//                     val input = grid(3)(
//                         0, 1, 2,
//                         3, 4, 5,
//                         6, 7, 8
//                     )
//                     val expected = grid(3)(
//                         9, 8, 7,
//                         6, 5, 4,
//                         3, 2, 1
//                     )
//                     val result = Color.InvertColors(
//                         maxColor = 9,
//                         preserveBackground = false,
//                         backgroundColor = 0
//                     )(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "symmetric inversion" in run {
//                     val input = grid(2)(
//                         2,
//                         7,
//                         5,
//                         4
//                     )
//                     // Invert: 2->7, 7->2, 5->4, 4->5
//                     val result1 =
//                         Color.InvertColors(
//                             maxColor = 9,
//                             preserveBackground = false,
//                             backgroundColor = 0
//                         )(input)
//                     val result2 =
//                         Color.InvertColors(
//                             maxColor = 9,
//                             preserveBackground = false,
//                             backgroundColor = 0
//                         )(result1)
//                     // Double inversion should return original
//                     assert(result2.cells == input.cells)

//                 }
//             }

//             "Background preservation" - {

//                 "preserve background=true" in run {
//                     val input = grid(3)(
//                         0, 1, 2,
//                         1, 0, 1,
//                         2, 1, 0
//                     )
//                     val expected = grid(3)(
//                         0, 8, 7,
//                         8, 0, 8,
//                         7, 8, 0
//                     )
//                     val result =
//                         Color.InvertColors(
//                             maxColor = 9,
//                             preserveBackground = true,
//                             backgroundColor = 0
//                         )(input)
//                     assert(result.cells == expected.cells)
//                     // Verify background (0) is preserved
//                     assert(input.cells.flatten.count(_ == 0) == result.cells.flatten.count(_ == 0))

//                 }

//                 "preserve background=false" in run {
//                     val input = grid(3)(
//                         0, 1, 2,
//                         1, 0, 1,
//                         2, 1, 0
//                     )
//                     val expected = grid(3)(
//                         9, 8, 7,
//                         8, 9, 8,
//                         7, 8, 9
//                     )
//                     val result =
//                         Color.InvertColors(
//                             maxColor = 9,
//                             preserveBackground = false,
//                             backgroundColor = 0
//                         )(input)

//                     assert(result.cells == expected.cells)
//                     // Verify background (0) is also inverted
//                     assert(!result.cells.flatten.contains(0))

//                 }
//             }

//             "Different max colors" - {

//                 "maxColor=9 standard" in run {
//                     val input = grid(2)(
//                         1,
//                         5,
//                         9,
//                         3
//                     )
//                     val expected = grid(2)(
//                         8,
//                         4,
//                         0,
//                         6
//                     )
//                     val result =
//                         Color.InvertColors(
//                             maxColor = 9,
//                             preserveBackground = false,
//                             backgroundColor = -1
//                         )(input)

//                     assert(result.cells == expected.cells)

//                 }

//                 "maxColor=9 for full palette" in run {
//                     val input = grid(2)(
//                         0,
//                         4,
//                         7,
//                         9
//                     )
//                     val expected = grid(2)(
//                         9,
//                         5,
//                         2,
//                         0
//                     )
//                     val result =
//                         Color.InvertColors(
//                             maxColor = 9,
//                             preserveBackground = false,
//                             backgroundColor = -1
//                         )(input)

//                     assert(result.cells == expected.cells)

//                 }
//             }

//             "Edge cases" - {

//                 "invert with maxColor=0" in run {
//                     val input = grid(2)(
//                         0,
//                         0,
//                         0,
//                         0
//                     )
//                     val expected = grid(2)(
//                         0,
//                         0,
//                         0,
//                         0
//                     )
//                     val result =
//                         Color.InvertColors(
//                             maxColor = 0,
//                             preserveBackground = false,
//                             backgroundColor = -1
//                         )(input)

//                     assert(result.cells == expected.cells)

//                 }

//                 "partial inversion with maxColor<9" in run {
//                     val input = grid(2)(
//                         1,
//                         3,
//                         5,
//                         0
//                     )
//                     val expected = grid(2)(
//                         4,
//                         2,
//                         0,
//                         5
//                     )
//                     // With maxColor=5: 5-1=4, 5-3=2, 5-5=0, 5-0=5
//                     val result =
//                         Color.InvertColors(
//                             maxColor = 5,
//                             preserveBackground = false,
//                             backgroundColor = -1
//                         )(input)

//                     assert(result.cells == expected.cells)

//                 }
//             }
//         }

//         "Binarize" - {

//             "Basic binarization" - {

//                 "convert non-zero to 1, zero to 0" in run {
//                     val input = grid(3)(
//                         1, 2, 0,
//                         0, 5, 0,
//                         7, 0, 3
//                     )
//                     val expected = grid(3)(
//                         1, 1, 0,
//                         0, 1, 0,
//                         1, 0, 1
//                     )
//                     val result =
//                         Color.BinarizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "all non-zero becomes 1" in run {
//                     val input = grid(3)(
//                         1, 2, 3,
//                         4, 5, 6,
//                         7, 8, 9
//                     )
//                     val expected = grid(3)(
//                         1, 1, 1,
//                         1, 1, 1,
//                         1, 1, 1
//                     )
//                     val result =
//                         Color.BinarizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "all zero remains 0" in run {
//                     val input = grid(3)(
//                         0, 0, 0,
//                         0, 0, 0,
//                         0, 0, 0
//                     )
//                     val expected = grid(3)(
//                         0, 0, 0,
//                         0, 0, 0,
//                         0, 0, 0
//                     )
//                     val result =
//                         Color.BinarizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)

//                 }
//             }

//             "Non-zero background" - {

//                 "background=5 treated as background" in run {
//                     val input = grid(3)(
//                         1, 5, 3,
//                         5, 2, 5,
//                         7, 5, 9
//                     )
//                     val expected = grid(3)(
//                         1, 0, 1,
//                         0, 1, 0,
//                         1, 0, 1
//                     )
//                     val result =
//                         Color.BinarizeColors(backgroundColor = 5)(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "checkerboard pattern preserved" in run {
//                     val input = grid(4)(
//                         0, 1, 0, 1,
//                         1, 0, 1, 0,
//                         0, 1, 0, 1,
//                         1, 0, 1, 0
//                     )
//                     // Since background is 0, all 1s become 1, all 0s stay 0
//                     val result =
//                         Color.BinarizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == input.cells)

//                 }
//             }

//             "Use for masking" - {

//                 "create mask from sparse pattern" in run {
//                     val input = grid(3)(
//                         6, 6, 0,
//                         6, 0, 0,
//                         0, 6, 6
//                     )
//                     val expected = grid(3)(
//                         1, 1, 0,
//                         1, 0, 0,
//                         0, 1, 1
//                     )
//                     val result =
//                         Color.BinarizeColors(backgroundColor = 0)(input)
//                     assert(result.cells == expected.cells)
//                     // Verify binary: only 0 and 1
//                     assert(result.cells.flatten.forall(c => c == 0 || c == 1))

//                 }
//             }
//         }
//     }
// end ColorTest
