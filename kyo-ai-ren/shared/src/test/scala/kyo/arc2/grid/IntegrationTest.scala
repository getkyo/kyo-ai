// package kyo.arc2.grid

// import kyo.*
// import kyo.arc2.TestUtils
// import kyo.arc2.TestUtils.*

// /** Integration and composition tests for all Arc2 operations. Tests operation chaining, idempotency, commutativity, reversibility, and
//   * performance characteristics across all op categories.
//   */
// class IntegrationTest extends Test:

//     "Integration and Composition" - {

//         "Operation chaining" - {

//             "geometry + color" in run {
//                 // Rotate grid, then recolor objects
//                 val input = grid(5)(
//                     1, 1, 1, 0, 0,
//                     1, 0, 0, 0, 0,
//                     0, 0, 0, 0, 0,
//                     0, 0, 0, 0, 0,
//                     0, 0, 0, 0, 0
//                 )
//                 // Rotate 90 degrees
//                 val rotated = ops.Geometry.Rotate(Rotation.R90)(input)
//                 // Then recolor objects in rotated grid
//                 val result = ops.Objects.RecolorObjects(
//                     objectIds = List(1),
//                     colorMapping = Map(1 -> 5)
//                 )(rotated)
//                 // Verify recoloring: all color 1 pixels should become color 5
//                 assert(!result.cells.flatten.contains(1), "No color 1 should remain after recoloring")
//                 assert(result.cells.flatten.count(_ == 5) == 4, "Should have exactly 4 pixels of color 5 (rotated L-shape)")
//                 // Verify grid dimensions unchanged
//                 assert(result.width == 5 && result.height == 5, "Grid dimensions should remain 5x5")
//                 // Verify the rotated L-shape is present somewhere in the grid
//                 val totalRecoloredPixels = result.cells.flatten.count(_ == 5)
//                 assert(totalRecoloredPixels == 4, "Should have 4 recolored pixels after rotation")
//             }

//             "morphology sequence" in run {
//                 // Dilate, then Erode (opening) - smooths edges
//                 val input = grid(7)(
//                     0, 0, 0, 0, 0, 0, 0,
//                     0, 1, 1, 1, 1, 0, 0,
//                     0, 1, 1, 1, 1, 0, 0,
//                     0, 1, 1, 1, 1, 0, 0,
//                     0, 1, 1, 1, 1, 0, 0,
//                     0, 0, 0, 0, 0, 0, 0,
//                     0, 0, 0, 0, 0, 0, 0
//                 )
//                 val originalPixelCount = input.cells.flatten.count(_ == 1)

//                 // Dilate first - should expand the shape
//                 val dilated = ops.Morphology.Dilate(
//                     iterations = 1,
//                     connectivity = Connectivity.FourConnected,
//                     targetColor = None
//                 )(input)
//                 // Then erode back - opening operation
//                 val result = ops.Morphology.Erode(
//                     iterations = 1,
//                     connectivity = Connectivity.FourConnected,
//                     targetColor = None
//                 )(dilated)
//                 val dilatedPixelCount = dilated.cells.flatten.count(_ == 1)
//                 assert(dilatedPixelCount > originalPixelCount, "Dilation should increase pixel count")

//                 // Opening should smooth edges but preserve core structure
//                 // The 2x2 center should remain
//                 assert(result.get(2, 2) == 1, "Center pixel (2,2) should survive opening")
//                 assert(result.get(2, 3) == 1, "Center pixel (2,3) should survive opening")
//                 assert(result.get(3, 2) == 1, "Center pixel (3,2) should survive opening")
//                 assert(result.get(3, 3) == 1, "Center pixel (3,3) should survive opening")

//                 // The result should be smaller than dilated but may differ from original
//                 val resultPixelCount = result.cells.flatten.count(_ == 1)
//                 assert(resultPixelCount <= dilatedPixelCount, "Erosion should not increase pixel count")
//                 assert(resultPixelCount > 0, "Opening should not eliminate entire object")

//                 // Grid dimensions should remain constant
//                 assert(result.width == 7 && result.height == 7, "Grid dimensions should be preserved")
//             }
//         }

//         "Idempotency tests" - {

//             "operations that should be idempotent" in run {
//                 // Test Flip - applying twice should return to original
//                 val input = grid(3)(
//                     1, 2, 3,
//                     4, 5, 6,
//                     7, 8, 9
//                 )
//                 val flipped1 = ops.Geometry.Flip(Axis.Horizontal)(input)
//                 val flipped2 = ops.Geometry.Flip(Axis.Horizontal)(flipped1)
//                 assert(flipped1.cells != input.cells, "First flip should change the grid")
//                 assert(flipped2.cells == input.cells, "Flip twice should return to original (idempotent)")
//                 // Verify specific values to ensure correct restoration
//                 assert(flipped2.get(0, 0) == 1 && flipped2.get(2, 2) == 9, "Values should match original positions")

//                 // Test ReplaceColor on already replaced grid
//                 val input2 = grid(3)(
//                     1, 1, 0,
//                     1, 0, 0,
//                     0, 0, 0
//                 )
//                 val replaced1 = ops.Color.ReplaceColors(Map(1 -> 5))(input2)
//                 val replaced2 = ops.Color.ReplaceColors(Map(1 -> 5))(replaced1)
//                 assert(!replaced1.cells.flatten.contains(1), "First replacement should change all 1s to 5s")
//                 assert(replaced1.cells.flatten.count(_ == 5) == 3, "Should have exactly 3 pixels of color 5")
//                 assert(replaced1.cells == replaced2.cells, "Second replacement should be no-op (idempotent)")
//                 assert(replaced2.cells.flatten.count(_ == 5) == 3, "Pixel count should remain stable")

//                 // Test RemoveObjects idempotency - removing same color twice
//                 val input3 = grid(5)(
//                     1, 1, 0, 2, 2,
//                     1, 1, 0, 2, 2,
//                     0, 0, 0, 0, 0,
//                     3, 3, 0, 0, 0,
//                     3, 3, 0, 0, 0
//                 )
//                 // Remove all pixels of color 1
//                 val removed1 = ops.Objects.RemoveObjects(List(1), 0)(input3)
//                 // Try to remove object ID 2 (which corresponds to color 2 in removed1)
//                 // After first removal, object IDs are re-assigned: color 2 becomes ID 1, color 3 becomes ID 2
//                 val removed2 = ops.Objects.RemoveObjects(List(2), 0)(removed1)
//                 // Try to remove object ID 2 again - should be idempotent since object 2 is already gone
//                 val removed3 = ops.Objects.RemoveObjects(List(2), 0)(removed2)
//                 assert(!removed1.cells.flatten.contains(1), "First removal should eliminate color 1")
//                 assert(removed1.cells.flatten.contains(2) && removed1.cells.flatten.contains(3), "Other colors should remain")
//                 assert(!removed2.cells.flatten.contains(3), "Second removal should eliminate color 3 (which is now object ID 2)")
//                 assert(removed2.cells == removed3.cells, "Third removal should be no-op (idempotent)")
//                 assert(
//                     removed3.cells.flatten.count(_ == 0) == removed2.cells.flatten.count(_ == 0),
//                     "Background pixel count should be stable"
//                 )
//             }
//         }

//         "Commutativity tests" - {

//             "operations that commute" in run {
//                 // ReplaceColor and Rotate should commute
//                 val input = grid(3)(
//                     1, 1, 0,
//                     1, 0, 0,
//                     0, 0, 2
//                 )

//                 // Order 1: ReplaceColor then Rotate
//                 val colored1        = ops.Color.ReplaceColors(Map(1 -> 5))(input)
//                 val colorThenRotate = ops.Geometry.Rotate(Rotation.R90)(colored1)

//                 // Order 2: Rotate then ReplaceColor
//                 val rotated2        = ops.Geometry.Rotate(Rotation.R90)(input)
//                 val rotateThenColor = ops.Color.ReplaceColors(Map(1 -> 5))(rotated2)
//                 // Verify commutativity: order doesn't matter
//                 assert(colorThenRotate.cells == rotateThenColor.cells, "ReplaceColor and Rotate should commute")
//                 assert(!colorThenRotate.cells.flatten.contains(1), "Color 1 should be replaced in both")
//                 assert(colorThenRotate.cells.flatten.count(_ == 5) == 3, "Both should have same number of recolored pixels")

//                 // FlipHorizontal and FlipVertical should commute
//                 val input2 = grid(4)(
//                     1, 2, 3, 4,
//                     5, 6, 7, 8,
//                     9, 0, 0, 0,
//                     0, 0, 0, 0
//                 )

//                 val h1     = ops.Geometry.Flip(Axis.Horizontal)(input2)
//                 val hThenV = ops.Geometry.Flip(Axis.Vertical)(h1)

//                 val v2     = ops.Geometry.Flip(Axis.Vertical)(input2)
//                 val vThenH = ops.Geometry.Flip(Axis.Horizontal)(v2)
//                 // Verify commutativity
//                 assert(hThenV.cells == vThenH.cells, "Horizontal and Vertical flips should commute")
//                 // Verify both produce 180° rotation effect
//                 assert(hThenV.get(0, 0) == input2.get(3, 3), "Top-left should equal original bottom-right")
//                 assert(hThenV.get(3, 3) == input2.get(0, 0), "Bottom-right should equal original top-left")
//             }

//             "operations that don't commute" in run {
//                 // Crop then Scale vs Scale then Crop - results differ
//                 val input = grid(5)(
//                     0, 0, 0, 0, 0,
//                     0, 1, 1, 0, 0,
//                     0, 1, 1, 0, 0,
//                     0, 0, 0, 0, 0,
//                     0, 0, 0, 0, 0
//                 )

//                 // Order 1: Crop (to 3x3) then Scale (2x)
//                 val cropped1      = ops.Geometry.Crop(x = 1, y = 1, width = 3, height = 3)(input)
//                 val cropThenScale = ops.Geometry.Scale(factorX = 2, factorY = 2)(cropped1)

//                 // Order 2: Scale (2x) then Crop (to 3x3)
//                 val scaled2       = ops.Geometry.Scale(factorX = 2, factorY = 2)(input)
//                 val scaleThenCrop = ops.Geometry.Crop(x = 1, y = 1, width = 3, height = 3)(scaled2)
//                 // These should be different
//                 assert(cropThenScale.cells != scaleThenCrop.cells)
//                 assert(cropThenScale.width != scaleThenCrop.width ||
//                     cropThenScale.height != scaleThenCrop.height)

//                 // Move then Rotate vs Rotate then Move - different final positions
//                 val input2 = grid(5)(
//                     0, 0, 0, 0, 0,
//                     0, 1, 1, 0, 0,
//                     0, 1, 1, 0, 0,
//                     0, 0, 0, 0, 0,
//                     0, 0, 0, 0, 0
//                 )

//                 // Move down, then rotate
//                 val moved1         = ops.Objects.MoveObjects(List(1), 0, 1, 0)(input2)
//                 val moveThenRotate = ops.Geometry.Rotate(Rotation.R90)(moved1)

//                 // Rotate, then move (but object position changes after rotation)
//                 val rotated2       = ops.Geometry.Rotate(Rotation.R90)(input2)
//                 val rotateThenMove = ops.Objects.MoveObjects(List(1), 0, 1, 0)(rotated2)
//                 // Order matters: rotation changes where the object ends up
//                 assert(moveThenRotate.cells != rotateThenMove.cells)
//             }
//         }

//         "Reversibility tests" - {

//             "reverse operations" in run {
//                 // Rotate R90 then R270 should return to original
//                 val input = grid(4)(
//                     1, 2, 3, 4,
//                     5, 6, 7, 8,
//                     9, 0, 0, 0,
//                     0, 0, 0, 1
//                 )
//                 val rotated  = ops.Geometry.Rotate(Rotation.R90)(input)
//                 val reversed = ops.Geometry.Rotate(Rotation.R270)(rotated)
//                 assert(reversed.cells == input.cells)

//                 // Flip twice returns to original
//                 val flipped        = ops.Geometry.Flip(Axis.Horizontal)(input)
//                 val backToOriginal = ops.Geometry.Flip(Axis.Horizontal)(flipped)
//                 assert(backToOriginal.cells == input.cells)

//                 // ReplaceColor can be reversed with another ReplaceColor
//                 val input2 = grid(3)(
//                     1, 1, 0,
//                     1, 0, 2,
//                     0, 2, 2
//                 )
//                 val recolored     = ops.Color.ReplaceColors(Map(1 -> 5))(input2)
//                 val reversedColor = ops.Color.ReplaceColors(Map(5 -> 1))(recolored)
//                 assert(reversedColor.cells == input2.cells)
//             }

//         }

//         "Edge case combinations" - {

//             "empty grid through pipeline" in run {
//                 // Test various operations on an empty (all background) grid
//                 val empty = emptyGrid(5, 5, 0)

//                 // Geometric operations should preserve empty grid
//                 val rotated = ops.Geometry.Rotate(Rotation.R90)(empty)
//                 val flipped = ops.Geometry.Flip(Axis.Horizontal)(empty)
//                 // Color operations on empty grid
//                 val recolored = ops.Color.ReplaceColors(Map(0 -> 5))(empty)
//                 // Morphology operations on empty grid (all background)
//                 val dilated = ops.Morphology.Dilate(
//                     iterations = 1,
//                     connectivity = Connectivity.FourConnected,
//                     targetColor = None
//                 )(empty)
//                 // Grid operations on empty
//                 val scaled = ops.Geometry.Scale(factorX = 2, factorY = 2)(empty)
//                 assert(rotated.cells.flatten.forall(_ == 0))
//                 assert(flipped.cells.flatten.forall(_ == 0))
//                 assert(recolored.cells.flatten.forall(_ == 5))
//                 assert(dilated.cells.flatten.forall(_ == 0))
//                 assert(scaled.width == 10 && scaled.height == 10)
//                 assert(scaled.cells.flatten.forall(_ == 0))
//             }

//             "single pixel through pipeline" in run {
//                 // Test operations on minimal 1x1 grid
//                 val singlePixel = grid(1)(5)

//                 // Rotate 1x1 - should be identity
//                 val rotated = ops.Geometry.Rotate(Rotation.R90)(singlePixel)
//                 // Flip 1x1 - should be identity
//                 val flipped = ops.Geometry.Flip(Axis.Horizontal)(singlePixel)
//                 // Recolor single pixel
//                 val recolored = ops.Color.ReplaceColors(Map(5 -> 9))(singlePixel)
//                 // Scale single pixel
//                 val scaled = ops.Geometry.Scale(factorX = 3, factorY = 3)(singlePixel)
//                 assert(rotated.cells == singlePixel.cells)
//                 assert(flipped.cells == singlePixel.cells)
//                 assert(recolored.cells == List(List(9)))
//                 assert(scaled.width == 3 && scaled.height == 3)
//                 assert(scaled.cells.flatten.forall(_ == 5))

//                 // Test slightly larger minimal case: 2x2 grid with one colored pixel
//                 val minimal = grid(2)(
//                     1,
//                     0,
//                     0,
//                     0
//                 )

//                 // Morphology on minimal grid
//                 val dilated = ops.Morphology.Dilate(
//                     iterations = 1,
//                     connectivity = Connectivity.FourConnected,
//                     targetColor = None
//                 )(minimal)
//                 // Should expand to neighbors
//                 assert(dilated.cells.flatten.count(_ == 1) > 1)
//             }
//         }

//         "Real ARC-AGI Examples" - {

//             "00576224 train[0] - Basic scaling" in run {
//                 // Real ARC-AGI 00576224 training example 1: 2x2 → 6x6 with tiling
//                 val input = Grid(List(
//                     List(7, 9),
//                     List(4, 3)
//                 ))

//                 // Scale the pattern to create a 6x6 grid (3x scaling)
//                 val scaled = ops.Geometry.Scale(factorX = 3, factorY = 3)(input)
//                 // Verify dimensions match expected output
//                 assert(scaled.width == 6 && scaled.height == 6, "Scaled grid should be 6x6")

//                 // Verify pattern is repeated through scaling
//                 assert(scaled.get(0, 0) == 7, "Scaled pixel matches original")
//                 assert(scaled.cells.flatten.count(_ == 7) > 0, "Should have color 7 pixels")
//             }

//             "007bbfb7 train[0] - Object replication to grid positions" in run {
//                 // Real ARC-AGI 007bbfb7 training example 1: 3x3 object gets replicated to 9x9 grid
//                 val input = Grid(List(
//                     List(6, 6, 0),
//                     List(6, 0, 0),
//                     List(0, 6, 6)
//                 ))

//                 // Scale up to 9x9
//                 val scaled = ops.Geometry.Scale(factorX = 3, factorY = 3)(input)
//                 // Verify dimensions
//                 assert(scaled.width == 9 && scaled.height == 9, "Scaled grid should be 9x9")

//                 // Verify color 6 pixels are present
//                 assert(scaled.cells.flatten.count(_ == 6) > 0, "Should have color 6 pixels after scaling")
//             }

//             "009d5c81 train[0] - Color replacement based on smaller pattern" in run {
//                 // Real ARC-AGI 009d5c81 training example 1: Replace color 8 with color 7
//                 // Input has a large color 8 pattern and a small color 1 pattern
//                 val inputRow0 = List(0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 0, 0)
//                 val inputRow1 = List(0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 0, 0, 0)
//                 val input = Grid(List(
//                     inputRow0,
//                     inputRow1,
//                     List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 8, 0),
//                     List(0, 0, 0, 0, 0, 8, 8, 8, 8, 0, 8, 8, 8, 0),
//                     List(0, 0, 0, 0, 8, 8, 0, 0, 8, 8, 8, 0, 8, 8),
//                     List(0, 0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 8, 0),
//                     List(0, 0, 0, 0, 0, 0, 8, 8, 0, 0, 0, 8, 8, 0),
//                     List(0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 8, 8, 0, 0),
//                     List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
//                     List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
//                     List(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
//                     List(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0),
//                     List(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
//                     List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
//                 ))

//                 // Replace color 8 with color 7
//                 val recolored = ops.Color.ReplaceColors(Map(8 -> 7))(input)
//                 // Verify replacement occurred
//                 assert(!recolored.cells.flatten.contains(8), "Color 8 should be replaced")
//                 assert(recolored.cells.flatten.count(_ == 7) > 0, "Should have color 7 pixels")
//                 assert(recolored.cells.flatten.contains(1), "Color 1 should still be present")
//             }

//             "007bbfb7 train[2] - Empty regions in replication" in run {
//                 // Real ARC-AGI 007bbfb7 training example 3: sparse pattern replication
//                 val input = Grid(List(
//                     List(0, 0, 0),
//                     List(0, 0, 2),
//                     List(2, 0, 2)
//                 ))

//                 // Replicate to 9x9
//                 val scaled = ops.Geometry.Scale(factorX = 3, factorY = 3)(input)
//                 // Verify dimensions
//                 assert(scaled.width == 9 && scaled.height == 9, "Should be 9x9")

//                 // Verify we have color 2 pixels
//                 assert(scaled.cells.flatten.count(_ == 2) > 0, "Should have color 2 pixels")

//                 // Verify we have lots of background
//                 assert(scaled.cells.flatten.count(_ == 0) > scaled.cells.flatten.count(_ == 2), "Should be mostly background")
//             }
//         }
//     }
// end IntegrationTest
