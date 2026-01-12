package kyo.arc2.grid.ops

import kyo.*
import kyo.arc2.{grid as _, *}
import kyo.arc2.TestUtils.*
import kyo.arc2.grid.*

class PhysicsTest extends Test:

    "Physics Operations" - {

        "ApplyGravity" - {

            "Direction variants" - {

                "down gravity objects fall to bottom" in run {
                    val input = grid(5)(
                        0, 1, 0, 2, 0,
                        0, 0, 0, 0, 0,
                        0, 3, 0, 0, 0,
                        0, 0, 0, 4, 0,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 1, 0, 2, 0,
                        0, 3, 0, 4, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "up gravity objects rise to top" in run {
                    val input = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 1, 0, 2, 0,
                        0, 0, 0, 0, 0,
                        0, 3, 0, 4, 0,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(5)(
                        0, 1, 0, 2, 0,
                        0, 3, 0, 4, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Up, backgroundColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "left gravity compacts to left edge" in run {
                    val input = grid(5)(
                        0, 1, 0, 2, 0,
                        0, 0, 3, 0, 0,
                        0, 0, 0, 4, 0,
                        0, 0, 0, 0, 0,
                        5, 0, 6, 0, 7
                    )
                    val expected = grid(5)(
                        1, 2, 0, 0, 0,
                        3, 0, 0, 0, 0,
                        4, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        5, 6, 7, 0, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Left, backgroundColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "right gravity compacts to right edge" in run {
                    val input = grid(5)(
                        0, 1, 0, 2, 0,
                        0, 0, 3, 0, 0,
                        0, 0, 0, 4, 0,
                        0, 0, 0, 0, 0,
                        5, 0, 6, 0, 7
                    )
                    val expected = grid(5)(
                        0, 0, 0, 1, 2,
                        0, 0, 0, 0, 3,
                        0, 0, 0, 0, 4,
                        0, 0, 0, 0, 0,
                        0, 0, 5, 6, 7
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Right, backgroundColor = 0)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Relative order preservation" - {

                "objects maintain stacking order" in run {
                    val input = grid(5)(
                        1, 0, 0, 0, 0,
                        2, 0, 0, 0, 0,
                        3, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        1, 0, 0, 0, 0,
                        2, 0, 0, 0, 0,
                        3, 0, 0, 0, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    assert(result.cells == expected.cells)

                }

                "no interpenetration" in run {
                    val input = grid(5)(
                        0, 0, 0, 0, 0,
                        0, 1, 0, 2, 0,
                        0, 0, 0, 0, 0,
                        0, 3, 0, 4, 0,
                        0, 0, 0, 0, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    // Each column should have its objects stacked
                    assert(result.get(1, 3) == 1)
                    assert(result.get(1, 4) == 3)
                    assert(result.get(3, 3) == 2)
                    assert(result.get(3, 4) == 4)

                }
            }

            "Background handling" - {

                "specified background moves correctly" in run {
                    val input = grid(5)(
                        9, 1, 9, 2, 9,
                        9, 9, 9, 9, 9,
                        9, 3, 9, 9, 9,
                        9, 9, 9, 4, 9,
                        9, 9, 9, 9, 9
                    )
                    val expected = grid(5)(
                        9, 9, 9, 9, 9,
                        9, 9, 9, 9, 9,
                        9, 9, 9, 9, 9,
                        9, 1, 9, 2, 9,
                        9, 3, 9, 4, 9
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 9)(input)
                    assert(result.cells == expected.cells)

                }

                "zero background standard case" in run {
                    val input = grid(3)(
                        0, 1, 0,
                        0, 0, 0,
                        0, 2, 0
                    )
                    val expected = grid(3)(
                        0, 0, 0,
                        0, 1, 0,
                        0, 2, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    assert(result.cells == expected.cells)

                }
            }

            "Edge cases" - {

                "empty grid unchanged" in run {
                    val input = grid(3)(
                        0, 0, 0,
                        0, 0, 0,
                        0, 0, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    assert(result.cells == input.cells)

                }

                "already settled objects" in run {
                    val input = grid(3)(
                        0, 0, 0,
                        0, 0, 0,
                        1, 2, 3
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    // Already at bottom, should not change
                    assert(result.cells == input.cells)

                }

                "mixed directions in sequence" in run {
                    val input = grid(5)(
                        0, 0, 1, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    // Apply down, then right
                    val afterDown = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    val result    = Physics.ApplyGravity(direction = Direction.Right, backgroundColor = 0)(afterDown)
                    // Should end up at bottom-right
                    assert(result.get(4, 4) == 1)
                }
            }

            "Complex patterns" - {

                "tetris-like falling" in run {
                    val input = grid(7)(
                        0, 0, 0, 0, 0, 0, 0,
                        0, 1, 1, 1, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0,
                        0, 0, 2, 2, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 3, 3, 3, 0,
                        0, 0, 0, 0, 0, 0, 0
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    // Each column should have its pieces stacked at bottom
                    // Column 1: has 1
                    assert(result.get(1, 6) == 1)
                    // Column 2: has 1, 2
                    assert(result.get(2, 5) == 1)
                    assert(result.get(2, 6) == 2)
                    // Column 3: has 1, 2, 3
                    assert(result.get(3, 4) == 1)
                    assert(result.get(3, 5) == 2)
                    assert(result.get(3, 6) == 3)

                }

                "gravity on scattered pixels" in run {
                    val input = grid(5)(
                        1, 0, 2, 0, 3,
                        0, 4, 0, 5, 0,
                        6, 0, 7, 0, 8,
                        0, 9, 0, 1, 0,
                        2, 0, 3, 0, 4
                    )
                    val result = Physics.ApplyGravity(direction = Direction.Down, backgroundColor = 0)(input)
                    // Column 0: 1, 6, 2 -> should stack at bottom
                    assert(result.get(0, 2) == 1)
                    assert(result.get(0, 3) == 6)
                    assert(result.get(0, 4) == 2)
                    // Column 2: 2, 7, 3 -> should stack at bottom
                    assert(result.get(2, 2) == 2)
                    assert(result.get(2, 3) == 7)
                    assert(result.get(2, 4) == 3)

                }
            }
        }

        "Compact" - {

            "Axis-specific compaction" - {

                "horizontal removes internal gaps" in run {
                    val input = grid(5)(
                        1, 0, 2, 0, 3,
                        0, 4, 0, 5, 0,
                        6, 0, 0, 7, 0,
                        0, 0, 8, 0, 9,
                        0, 0, 0, 0, 0
                    )
                    val expected = grid(5)(
                        1, 2, 3, 0, 0,
                        4, 5, 0, 0, 0,
                        6, 7, 0, 0, 0,
                        8, 9, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    val result = Physics.Compact(axis = Axis.Horizontal, backgroundColor = 0)(input)
                    // Horizontal compaction moves everything left
                    assert(result.cells == expected.cells)

                }

                "vertical removes internal gaps" in run {
                    val input = grid(5)(
                        1, 0, 2, 0, 3,
                        0, 4, 0, 5, 0,
                        6, 0, 7, 0, 8,
                        0, 9, 0, 1, 0,
                        2, 0, 3, 0, 4
                    )
                    val expected = grid(5)(
                        1, 4, 2, 5, 3,
                        6, 9, 7, 1, 8,
                        2, 0, 3, 0, 4,
                        0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0
                    )
                    val result = Physics.Compact(axis = Axis.Vertical, backgroundColor = 0)(input)
                    // Vertical compaction moves everything up
                    assert(result.cells == expected.cells)

                }
            }

            "Difference from gravity" - {

                "compact is unidirectional" in run {
                    val input = grid(5)(
                        0, 1, 0, 2, 0,
                        0, 0, 0, 0, 0,
                        0, 3, 0, 4, 0,
                        0, 0, 0, 0, 0,
                        0, 5, 0, 6, 0
                    )
                    // Horizontal compact always moves left
                    val compactH = Physics.Compact(axis = Axis.Horizontal, backgroundColor = 0)(input)
                    // Vertical compact always moves up
                    val compactV = Physics.Compact(axis = Axis.Vertical, backgroundColor = 0)(input)
                    // Horizontal: all non-zero should be on left
                    assert(compactH.get(0, 0) == 1)
                    assert(compactH.get(1, 0) == 2)
                    assert(compactH.get(0, 2) == 3)
                    assert(compactH.get(1, 2) == 4)

                    // Vertical: all non-zero should be on top
                    assert(compactV.get(1, 0) == 1)
                    assert(compactV.get(1, 1) == 3)
                    assert(compactV.get(1, 2) == 5)
                    assert(compactV.get(3, 0) == 2)
                }

                "no directional variant" in run {
                    val input = grid(3)(
                        0, 1, 0,
                        0, 0, 0,
                        0, 2, 0
                    )
                    // Compact horizontal is always left (no right option)
                    val result = Physics.Compact(axis = Axis.Horizontal, backgroundColor = 0)(input)
                    // Compact vertical is always up (no down option)
                    val resultV = Physics.Compact(axis = Axis.Vertical, backgroundColor = 0)(input)
                    assert(result.get(0, 0) == 1)
                    assert(result.get(0, 2) == 2)
                    assert(resultV.get(1, 0) == 1)
                    assert(resultV.get(1, 1) == 2)
                }
            }

            "Preservation properties" - {

                "relative order maintained" in run {
                    val input = grid(5)(
                        1, 0, 2, 0, 3,
                        0, 0, 0, 0, 0,
                        4, 0, 5, 0, 6,
                        0, 0, 0, 0, 0,
                        7, 0, 8, 0, 9
                    )
                    val result = Physics.Compact(axis = Axis.Horizontal, backgroundColor = 0)(input)
                    // Row 0: 1, 2, 3 should be in that order
                    assert(result.get(0, 0) == 1)
                    assert(result.get(1, 0) == 2)
                    assert(result.get(2, 0) == 3)
                    // Row 2: 4, 5, 6 should be in that order
                    assert(result.get(0, 2) == 4)
                    assert(result.get(1, 2) == 5)
                    assert(result.get(2, 2) == 6)

                }

                "object shapes unchanged" in run {
                    val input = grid(7)(
                        0, 0, 1, 1, 0, 0, 0,
                        0, 0, 1, 1, 0, 0, 0,
                        0, 0, 0, 0, 0, 2, 0,
                        0, 0, 0, 0, 0, 2, 0,
                        0, 0, 0, 0, 0, 2, 0,
                        3, 3, 3, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0
                    )
                    val result = Physics.Compact(axis = Axis.Horizontal, backgroundColor = 0)(input)
                    // Horizontal compact moves everything left per row
                    // 2×2 square of 1s: each row has [1,1] which stays together
                    assert(result.get(0, 0) == 1)
                    assert(result.get(1, 0) == 1)
                    assert(result.get(0, 1) == 1)
                    assert(result.get(1, 1) == 1)
                    // Single 2 in each of rows 2,3,4 - moves to left
                    assert(result.get(0, 2) == 2)
                    assert(result.get(0, 3) == 2)
                    assert(result.get(0, 4) == 2)
                    // 3×1 row of 3s stays together on left
                    assert(result.get(0, 5) == 3)
                    assert(result.get(1, 5) == 3)
                    assert(result.get(2, 5) == 3)

                }
            }
        }
    }
end PhysicsTest
