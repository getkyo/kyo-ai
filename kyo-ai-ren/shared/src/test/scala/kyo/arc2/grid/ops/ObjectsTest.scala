// package kyo.arc2.grid.ops

// import kyo.*
// import kyo.arc2.{grid as _, *}
// import kyo.arc2.TestUtils.*
// import kyo.arc2.grid.*

// class ObjectsTest extends Test:

//     "Objects Operations" - {

//         "MoveObjects" - {

//             "Basic movement" - {

//                 "move object right" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         0, 0, 1, 1, 0,
//                         0, 0, 1, 1, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1),
//                         deltaX = 2,
//                         deltaY = 0,
//                         fillBackground = 0
//                     )(input)
//                     // Verify complete grid match
//                     assert(result.cells == expected.cells, "Moved grid should match expected pattern")
//                     // Verify object moved to correct position
//                     assert(result.get(2, 0) == 1 && result.get(3, 0) == 1, "Object should be at new X position")
//                     assert(result.get(2, 1) == 1 && result.get(3, 1) == 1, "Object should maintain 2x2 shape")
//                     // Verify original position cleared
//                     assert(result.get(0, 0) == 0 && result.get(1, 0) == 0, "Original position should be filled with background")
//                     assert(result.get(0, 1) == 0 && result.get(1, 1) == 0, "Original position should be completely cleared")
//                     // Verify pixel count preserved
//                     assert(result.cells.flatten.count(_ == 1) == 4, "Object should maintain 4 pixels after move")
//                     // Verify grid dimensions unchanged
//                     assert(result.width == 5 && result.height == 5, "Grid dimensions should remain 5x5")

//                 }

//                 "move object down" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1),
//                         deltaX = 0,
//                         deltaY = 2,
//                         fillBackground = 0
//                     )(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "move object diagonally" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 1, 1, 0,
//                         0, 0, 1, 1, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1),
//                         deltaX = 2,
//                         deltaY = 2,
//                         fillBackground = 0
//                     )(input)
//                     assert(result.cells == expected.cells)

//                 }
//             }

//             "Multiple objects" - {

//                 "move specific object among many" in run {
//                     val input = grid(7)(
//                         1, 1, 0, 2, 2, 0, 0,
//                         1, 1, 0, 2, 2, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(2),
//                         deltaX = 2,
//                         deltaY = 1,
//                         fillBackground = 0
//                     )(input)
//                     // Object 1 unchanged
//                     assert(result.get(0, 0) == 1)
//                     assert(result.get(1, 0) == 1)
//                     // Object 2 moved
//                     assert(result.get(5, 1) == 2)
//                     assert(result.get(6, 1) == 2)
//                     // Object 3 unchanged
//                     assert(result.get(0, 3) == 3)
//                     assert(result.get(1, 3) == 3)

//                 }

//                 "move multiple objects together" in run {
//                     val input = grid(7)(
//                         1, 1, 0, 2, 2, 0, 0,
//                         1, 1, 0, 2, 2, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1, 2),
//                         deltaX = 0,
//                         deltaY = 3,
//                         fillBackground = 0
//                     )(input)
//                     // Object 1 moved down
//                     assert(result.get(0, 3) == 1)
//                     assert(result.get(1, 3) == 1)
//                     // Object 2 moved down
//                     assert(result.get(3, 3) == 2)
//                     assert(result.get(4, 3) == 2)
//                     // Object 3 unchanged
//                     assert(result.get(0, 3) == 1 || result.get(0, 3) == 3)

//                 }
//             }

//             "Boundary behavior" - {

//                 "move to edge" in run {
//                     val input = grid(5)(
//                         0, 0, 1, 1, 0,
//                         0, 0, 1, 1, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         0, 0, 0, 1, 1,
//                         0, 0, 0, 1, 1,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1),
//                         deltaX = 1,
//                         deltaY = 0,
//                         fillBackground = 0
//                     )(input)
//                     assert(result.cells == expected.cells)

//                 }

//                 "move beyond edge clips object" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1),
//                         deltaX = 10,
//                         deltaY = 0,
//                         fillBackground = 0
//                     )(input)
//                     // Object moved completely off grid
//                     assert(result.cells.flatten.forall(_ == 0))

//                 }

//                 "negative movement" in run {
//                     val input = grid(5)(
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 1, 1, 0,
//                         0, 0, 1, 1, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1),
//                         deltaX = -2,
//                         deltaY = -2,
//                         fillBackground = 0
//                     )(input)
//                     assert(result.cells == expected.cells)

//                 }
//             }

//             "Zero movement" - {

//                 "deltaX=0, deltaY=0 preserves grid" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(1, 2, 3),
//                         deltaX = 0,
//                         deltaY = 0,
//                         fillBackground = 0
//                     )(input)
//                     assert(result.cells == input.cells)

//                 }
//             }

//             "Non-existent object ID" - {

//                 "moving non-existent ID is no-op" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.MoveObjects(
//                         objectIds = List(99),
//                         deltaX = 3,
//                         deltaY = 3,
//                         fillBackground = 0
//                     )(input)
//                     assert(result.cells == input.cells)

//                 }
//             }
//         }

//         "RemoveObjects" - {

//             "Single object removal" - {

//                 "remove specific object" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val result = Objects.RemoveObjects(
//                         objectIds = List(2),
//                         fillColor = 0
//                     )(input)
//                     assert(result.cells == expected.cells)
//                     assert(!result.cells.flatten.contains(2))

//                 }
//             }

//             "Multiple objects removal" - {

//                 "remove multiple objects at once" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 4, 4,
//                         3, 3, 0, 4, 4
//                     )
//                     val expected = grid(5)(
//                         0, 0, 0, 2, 2,
//                         0, 0, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 4, 4,
//                         0, 0, 0, 4, 4
//                     )
//                     val result = Objects.RemoveObjects(
//                         objectIds = List(1, 3),
//                         fillColor = 0
//                     )(input)
//                     assert(result.cells == expected.cells)
//                     assert(!result.cells.flatten.contains(1))
//                     assert(!result.cells.flatten.contains(3))
//                     assert(result.cells.flatten.contains(2))
//                     assert(result.cells.flatten.contains(4))

//                 }
//             }

//             "Fill color variations" - {

//                 "fill with non-zero color" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.RemoveObjects(
//                         objectIds = List(1),
//                         fillColor = 9
//                     )(input)
//                     assert(result.get(0, 0) == 9)
//                     assert(result.get(1, 0) == 9)
//                     assert(result.get(0, 1) == 9)
//                     assert(result.get(1, 1) == 9)
//                     assert(!result.cells.flatten.contains(1))
//                     assert(result.cells.flatten.contains(2))

//                 }
//             }

//             "Non-existent object" - {

//                 "removing non-existent object is no-op" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.RemoveObjects(
//                         objectIds = List(99),
//                         fillColor = 0
//                     )(input)
//                     assert(result.cells == input.cells)

//                 }
//             }
//         }

//         "RecolorObjects" - {

//             "Single object recoloring" - {

//                 "recolor one object" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         5, 5, 0, 2, 2,
//                         5, 5, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val result = Objects.RecolorObjects(
//                         objectIds = List(1),
//                         colorMapping = Map(1 -> 5)
//                     )(input)
//                     assert(result.cells == expected.cells)
//                     assert(!result.cells.flatten.contains(1))
//                     assert(result.cells.flatten.contains(5))

//                 }
//             }

//             "Multiple objects recoloring" - {

//                 "recolor multiple objects differently" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val result = Objects.RecolorObjects(
//                         objectIds = List(1, 2, 3),
//                         colorMapping = Map(1 -> 5, 2 -> 6, 3 -> 7)
//                     )(input)
//                     assert(result.get(0, 0) == 5)
//                     assert(result.get(3, 0) == 6)
//                     assert(result.get(0, 3) == 7)
//                     assert(!result.cells.flatten.contains(1))
//                     assert(!result.cells.flatten.contains(2))
//                     assert(!result.cells.flatten.contains(3))

//                 }
//             }

//             "Partial mapping" - {

//                 "only colors in mapping are changed" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.RecolorObjects(
//                         objectIds = List(1, 2),
//                         colorMapping = Map(1 -> 5)
//                     )(input)
//                     assert(result.get(0, 0) == 5)
//                     assert(result.get(1, 0) == 5)
//                     // Color 2 unchanged (not in mapping)
//                     assert(result.get(3, 0) == 2)
//                     assert(result.get(4, 0) == 2)

//                 }
//             }

//             "Non-existent object" - {

//                 "recoloring non-existent object is no-op" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.RecolorObjects(
//                         objectIds = List(99),
//                         colorMapping = Map(99 -> 5)
//                     )(input)
//                     assert(result.cells == input.cells)

//                 }
//             }
//         }

//         "CopyObjects" - {

//             "Basic copying" - {

//                 "copy object to new location" in run {
//                     val input = grid(7)(
//                         1, 1, 0, 0, 0, 0, 0,
//                         1, 1, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0
//                     )
//                     val expected = grid(7)(
//                         1, 1, 0, 0, 0, 0, 0,
//                         1, 1, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 1, 1, 0,
//                         0, 0, 0, 0, 1, 1, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0
//                     )
//                     val result = Objects.CopyObject(
//                         sourceObjectId = 1,
//                         destX = 4,
//                         destY = 3
//                     )(input)
//                     assert(result.cells == expected.cells)
//                     // Original should remain
//                     assert(result.get(0, 0) == 1)
//                     assert(result.get(1, 0) == 1)
//                     // Copy should exist
//                     assert(result.get(4, 3) == 1)
//                     assert(result.get(5, 3) == 1)

//                 }
//             }

//             "Multiple copies" - {

//                 "copy multiple objects" in run {
//                     val input = grid(7)(
//                         1, 1, 0, 2, 2, 0, 0,
//                         1, 1, 0, 2, 2, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0, 0, 0
//                     )
//                     // Copy object 1 down by 4
//                     val step1 = Objects.CopyObject(sourceObjectId = 1, destX = 0, destY = 4)(input)
//                     // Copy object 2 down by 4
//                     val result = Objects.CopyObject(sourceObjectId = 2, destX = 3, destY = 4)(step1)
//                     // Originals remain
//                     assert(result.get(0, 0) == 1)
//                     assert(result.get(3, 0) == 2)
//                     // Copies exist
//                     assert(result.get(0, 4) == 1)
//                     assert(result.get(3, 4) == 2)
//                 }
//             }

//             "Zero delta" - {

//                 "destX=0, destY=0 duplicates in place" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.CopyObject(
//                         sourceObjectId = 1,
//                         destX = 0,
//                         destY = 0
//                     )(input)
//                     // Should be unchanged (copy overlaps original)
//                     assert(result.cells == input.cells)

//                 }
//             }

//             "Non-existent object" - {

//                 "copying non-existent object is no-op" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.CopyObject(
//                         sourceObjectId = 99,
//                         destX = 2,
//                         destY = 2
//                     )(input)
//                     assert(result.cells == input.cells)

//                 }
//             }
//         }

//         "IsolateObject" - {

//             "Basic extraction" - {

//                 "extract single object" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         3, 3, 0, 0, 0,
//                         3, 3, 0, 0, 0
//                     )
//                     val expected = grid(5)(
//                         1, 1, 0, 0, 0,
//                         1, 1, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.IsolateObject(
//                         objectId = 1,
//                         backgroundColor = 0
//                     )(input)
//                     assert(result.cells == expected.cells)
//                     assert(result.cells.flatten.contains(1))
//                     assert(!result.cells.flatten.contains(2))
//                     assert(!result.cells.flatten.contains(3))

//                 }
//             }

//             "Different background" - {

//                 "extract with non-zero background" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.IsolateObject(
//                         objectId = 1,
//                         backgroundColor = 9
//                     )(input)
//                     assert(result.get(0, 0) == 1)
//                     assert(result.get(1, 0) == 1)
//                     assert(result.get(2, 0) == 9)
//                     assert(result.get(3, 0) == 9)

//                 }
//             }

//             "Non-existent object" - {

//                 "extracting non-existent object returns background" in run {
//                     val input = grid(5)(
//                         1, 1, 0, 2, 2,
//                         1, 1, 0, 2, 2,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0,
//                         0, 0, 0, 0, 0
//                     )
//                     val result = Objects.IsolateObject(
//                         objectId = 99,
//                         backgroundColor = 0
//                     )(input)
//                     assert(result.cells.flatten.forall(_ == 0))

//                 }
//             }
//         }

//     }
// end ObjectsTest
