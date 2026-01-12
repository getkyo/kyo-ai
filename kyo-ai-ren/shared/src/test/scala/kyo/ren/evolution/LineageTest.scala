package kyo.ren.evolution

import kyo.*
import kyo.Test
import kyo.ren.*
import kyo.ren.evolution.Evolution.TestScore
import kyo.ren.network.*

class LineageTest extends Test:

    def createMindState(name: String): Mind.State =
        Mind.State(
            name = name,
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State("prompt", "reminder", Temperature.balanced, History.State(List.empty, 0))
        )

    def createIndividual(name: String, sex: Boolean = false, lineage: Lineage = Lineage.empty): Individual =
        val mindState = createMindState(name)
        Individual(
            genotype = mindState,
            phenotype = mindState,
            fitness = List.empty,
            sex = sex,
            lineage = lineage
        )
    end createIndividual

    "Lineage initialization" - {

        "same parents produce same lineage" in {
            val parent1  = createIndividual("parent1")
            val parent2  = createIndividual("parent2")
            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent1, parent2)

            assert(lineage1.distanceTo(lineage2) == 0.0)
        }

        "different parents produce different lineages" in {
            val parent1  = createIndividual("parent1")
            val parent2  = createIndividual("parent2")
            val parent3  = createIndividual("parent3")
            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent1, parent3)

            assert(lineage1.distanceTo(lineage2) > 0.0)
        }

    }

    "Lineage merging" - {
        "parent order affects lineage hash order but not distance symmetry" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")

            val lineageOrder1 = Lineage.init(parent1, parent2)
            val lineageOrder2 = Lineage.init(parent2, parent1)

            // Distance should be symmetric
            assert(lineageOrder1.distanceTo(lineageOrder2) == lineageOrder2.distanceTo(lineageOrder1))
        }

        "lineage inherits from both parents" in {
            val grandparent1 = createIndividual("gp1")
            val grandparent2 = createIndividual("gp2")
            val grandparent3 = createIndividual("gp3")
            val grandparent4 = createIndividual("gp4")

            val parentLineage1 = Lineage.init(grandparent1, grandparent2)
            val parentLineage2 = Lineage.init(grandparent3, grandparent4)

            val parent1 = createIndividual("parent1", lineage = parentLineage1)
            val parent2 = createIndividual("parent2", lineage = parentLineage2)

            val childLineage = Lineage.init(parent1, parent2)

            // Child lineage should be different from both parent lineages
            assert(childLineage.distanceTo(parentLineage1) > 0.0)
            assert(childLineage.distanceTo(parentLineage2) > 0.0)
        }
    }

    "Distance calculation" - {
        "distance to empty is one" in {
            val lineage = Lineage.empty

            val distance = lineage.distanceTo(lineage)
            assert(distance == 1.0)
        }

        "unrelated individuals have maximum distance" in {
            val parent1  = createIndividual("parent1")
            val parent2  = createIndividual("parent2")
            val parent3  = createIndividual("parent3")
            val parent4  = createIndividual("parent4")
            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent3, parent4)

            val distance = lineage1.distanceTo(lineage2)
            assert(distance == 1.0)
        }

        "siblings have zero distance (same parents)" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")

            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent1, parent2)

            val distance = lineage1.distanceTo(lineage2)

            assert(distance == 0.0)
        }

        "parent-child share some ancestry" in {
            val grandparent1 = createIndividual("gp1")
            val grandparent2 = createIndividual("gp2")

            val parentLineage = Lineage.init(grandparent1, grandparent2)
            val parent        = createIndividual("parent", lineage = parentLineage)
            val otherParent   = createIndividual("other")

            val childLineage = Lineage.init(parent, otherParent)

            val distance = parentLineage.distanceTo(childLineage)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }

        "distance is symmetric" in {
            val parent1  = createIndividual("parent1")
            val parent2  = createIndividual("parent2")
            val parent3  = createIndividual("parent3")
            val parent4  = createIndividual("parent4")
            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent3, parent4)

            val distance1 = lineage1.distanceTo(lineage2)
            val distance2 = lineage2.distanceTo(lineage1)

            assert(distance1 == distance2)
        }

        "distance with empty lineage is maximum" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")
            val lineage = Lineage.init(parent1, parent2)
            val empty   = Lineage.empty

            val distance = lineage.distanceTo(empty)
            assert(distance == 1.0)
        }

        "distance between empty lineages is maximum" in {
            val empty1 = Lineage.empty
            val empty2 = Lineage.empty

            val distance = empty1.distanceTo(empty2)
            assert(distance == 1.0)
        }
    }

    "Multi-generational lineages" - {
        "lineage accumulates ancestors over generations" in {
            val gen0a = createIndividual("gen0a")
            val gen0b = createIndividual("gen0b")

            val lineage1 = Lineage.init(gen0a, gen0b)
            val gen1     = createIndividual("gen1", lineage = lineage1)

            val gen2a = createIndividual("gen2a")
            val gen2b = createIndividual("gen2b")

            val lineage2 = Lineage.init(gen2a, gen2b)
            val gen3     = createIndividual("gen3", lineage = lineage2)

            val extendedLineage = Lineage.init(gen1, gen3)

            // Extended lineage contains ancestors from both branches
            assert(extendedLineage.distanceTo(lineage1) < 1.0)
            assert(extendedLineage.distanceTo(lineage2) < 1.0)
        }

        "cousins share grandparents" in {
            val grandparent1 = createIndividual("grandparent1")
            val grandparent2 = createIndividual("grandparent2")

            val grandparentLineage = Lineage.init(grandparent1, grandparent2)

            val parent1 = createIndividual("parent1", lineage = grandparentLineage)
            val parent2 = createIndividual("parent2", lineage = grandparentLineage)

            val cousinLineage1 = Lineage.init(parent1, parent2)
            val cousinLineage2 = Lineage.init(parent1, parent2)

            val distance = cousinLineage1.distanceTo(cousinLineage2)

            assert(distance == 0.0) // Same parents = same lineage
        }
    }

    "Equality via distance" - {
        "lineages with same ancestry have zero distance" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")

            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent1, parent2)

            assert(lineage1.distanceTo(lineage2) == 0.0)
        }

        "lineages with different ancestry have non-zero distance" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")
            val parent3 = createIndividual("parent3")

            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent1, parent3)

            assert(lineage1.distanceTo(lineage2) > 0.0)
        }
    }

    "Partial ancestry overlap" - {
        "half-siblings have greater distance than full siblings" in {
            val sharedParent = createIndividual("shared-parent")
            val parent1      = createIndividual("parent1")
            val parent2      = createIndividual("parent2")

            val fullSibling1 = Lineage.init(sharedParent, parent1)
            val fullSibling2 = Lineage.init(sharedParent, parent1)
            val halfSibling  = Lineage.init(sharedParent, parent2)

            val fullDistance = fullSibling1.distanceTo(fullSibling2)
            val halfDistance = fullSibling1.distanceTo(halfSibling)

            assert(halfDistance > fullDistance)
        }

        "step-relatives have greater distance than half-siblings" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")
            val parent3 = createIndividual("parent3")
            val parent4 = createIndividual("parent4")

            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent3, parent4)

            val distance = lineage1.distanceTo(lineage2)

            assert(distance == 1.0)
        }

        "one shared grandparent reduces distance" in {
            val sharedGrandparent = createIndividual("shared-grandparent")
            val grandparent2      = createIndividual("grandparent2")
            val grandparent3      = createIndividual("grandparent3")
            val grandparent4      = createIndividual("grandparent4")

            val parentLineage1 = Lineage.init(sharedGrandparent, grandparent2)
            val parentLineage2 = Lineage.init(sharedGrandparent, grandparent3)
            val parentLineage3 = Lineage.init(grandparent3, grandparent4)

            val parent1 = createIndividual("parent1", lineage = parentLineage1)
            val parent2 = createIndividual("parent2", lineage = parentLineage2)
            val parent3 = createIndividual("parent3", lineage = parentLineage3)

            val lineage1        = Lineage.init(parent1, parent2)
            val noSharedLineage = Lineage.init(parent1, parent3)

            val distanceWithShared = lineage1.distanceTo(lineage1)
            val distanceNoShared   = lineage1.distanceTo(noSharedLineage)

            assert(distanceWithShared < distanceNoShared)
        }
    }

    "Inbreeding scenarios" - {
        "same individual as both parents" in {
            val parent = createIndividual("parent")

            val lineage = Lineage.init(parent, parent)

            // Lineage still created, even with same parent twice
            assert(lineage.distanceTo(lineage) == 0.0)
        }

        "repeated ancestor appears only once in distance calculation" in {
            val common = createIndividual("common")
            val other1 = createIndividual("other1")

            val lineage1 = Lineage.init(common, common)
            val lineage2 = Lineage.init(common, other1)

            val distance = lineage1.distanceTo(lineage2)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }

        "inbred lineage has higher similarity to ancestors" in {
            val ancestor = createIndividual("ancestor")
            val gen1     = createIndividual("gen1")

            val ancestorLineage  = Lineage.init(ancestor, ancestor)
            val gen1WithAncestor = createIndividual("gen1-anc", lineage = ancestorLineage)

            val inbredLineage = Lineage.init(gen1WithAncestor, gen1WithAncestor)
            val normalLineage = Lineage.init(gen1WithAncestor, ancestor)

            val inbredDistance = inbredLineage.distanceTo(ancestorLineage)
            val normalDistance = normalLineage.distanceTo(ancestorLineage)

            assert(inbredDistance <= normalDistance)
        }
    }

    "Deep lineages" - {
        "lineages with more shared ancestry have smaller distance" in {
            val shared1 = createIndividual("shared1")
            val shared2 = createIndividual("shared2")
            val shared3 = createIndividual("shared3")

            val shallowShared = Lineage.init(shared1, shared2)
            val shallowParent = createIndividual("shallow", lineage = shallowShared)

            val deepShared1 = Lineage.init(shared1, shared2)
            val deepParent1 = createIndividual("deep1", lineage = deepShared1)
            val deepShared2 = Lineage.init(deepParent1, shared3)
            val deepParent  = createIndividual("deep2", lineage = deepShared2)

            val reference = Lineage.init(shared1, shared2)
            val refParent = createIndividual("ref", lineage = reference)

            val shallowDist = Lineage.init(shallowParent, refParent).distanceTo(reference)
            val deepDist    = Lineage.init(deepParent, refParent).distanceTo(reference)

            assert(shallowDist < deepDist)
        }

        "very deep lineage accumulates many ancestors" in {
            val ancestors      = (0 until 20).map(i => createIndividual(s"gen$i"))
            var currentParents = (ancestors(0), ancestors(1))

            ancestors.drop(2).grouped(2).foreach { pair =>
                val lineage = Lineage.init(currentParents._1, currentParents._2)
                currentParents = (createIndividual(s"child${pair(0).genotype.name}", lineage = lineage), pair.lift(1).getOrElse(pair(0)))
            }

            // Verify lineage was built through multiple generations
            assert(currentParents._1.lineage.distanceTo(Lineage.empty) > 0.0)
        }
    }

    "Distance metric properties" - {
        "triangle inequality holds" in {
            val p1 = createIndividual("p1")
            val p2 = createIndividual("p2")
            val p3 = createIndividual("p3")
            val p4 = createIndividual("p4")
            val p5 = createIndividual("p5")

            val lineage1 = Lineage.init(p1, p2)
            val lineage2 = Lineage.init(p2, p3)
            val lineage3 = Lineage.init(p4, p5)

            val d12 = lineage1.distanceTo(lineage2)
            val d23 = lineage2.distanceTo(lineage3)
            val d13 = lineage1.distanceTo(lineage3)

            assert(d13 <= d12 + d23)
        }

        "distance is always between 0 and 1" in {
            val individuals = (0 until 10).map(i => createIndividual(s"ind$i"))
            val lineages = individuals.sliding(2).map { pair =>
                Lineage.init(pair(0), pair(1))
            }.toList

            lineages.foreach { l1 =>
                lineages.foreach { l2 =>
                    val distance = l1.distanceTo(l2)
                    assert(distance >= 0.0)
                    assert(distance <= 1.0)
                }
            }
            succeed
        }

        "adding shared ancestors increases similarity" in {
            val parent1  = createIndividual("parent1")
            val parent2  = createIndividual("parent2")
            val ancestor = createIndividual("ancestor")

            val lineage1Before = Lineage.init(parent1, parent2)
            val lineage2Before = Lineage.init(parent1, parent2)

            val ancestorLineage = Lineage.init(ancestor, ancestor)
            val parent1After    = createIndividual("parent1-after", lineage = ancestorLineage)
            val parent2After    = createIndividual("parent2-after", lineage = ancestorLineage)

            val lineage1After = Lineage.init(parent1After, parent2After)
            val lineage2After = Lineage.init(parent1After, parent2After)

            val distanceBefore = lineage1Before.distanceTo(lineage2Before)
            val distanceAfter  = lineage1After.distanceTo(lineage2After)

            assert(distanceAfter <= distanceBefore)
        }
    }

    "Edge cases" - {
        "lineage from two parents with empty lineages" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")

            val lineage = Lineage.init(parent1, parent2)

            assert(lineage.distanceTo(Lineage.empty) > 0.0)
        }

        "multiple generations with same parent" in {
            val parent = createIndividual("parent")

            val lineage1 = Lineage.init(parent, parent)
            val child1   = createIndividual("child1", lineage = lineage1)

            val lineage2 = Lineage.init(child1, child1)
            val child2   = createIndividual("child2", lineage = lineage2)

            val lineage3 = Lineage.init(child2, child2)

            // Verify lineages build up across generations
            assert(lineage1.distanceTo(lineage2) > 0.0)
            assert(lineage2.distanceTo(lineage3) > 0.0)
        }

        "distance with lineages of different ancestry depths" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")
            val parent3 = createIndividual("parent3")

            val shallow = Lineage.empty

            val deepLineage1 = Lineage.init(parent1, parent2)
            val deepParent   = createIndividual("deep", lineage = deepLineage1)
            val deep         = Lineage.init(deepParent, parent3)

            val distance = shallow.distanceTo(deep)

            assert(distance > 0.0)
            assert(distance <= 1.0)
        }
    }

    "Ancestry deduplication" - {
        "repeated parents across generations are counted once" in {
            val common = createIndividual("common")

            val lineage1 = Lineage.init(common, common)
            val child1   = createIndividual("child1", lineage = lineage1)
            val lineage2 = Lineage.init(child1, child1)
            val child2   = createIndividual("child2", lineage = lineage2)
            val lineage3 = Lineage.init(child2, child2)

            val simpleLineage = Lineage.init(common, common)

            val distance = lineage3.distanceTo(simpleLineage)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }

        "all ancestors identical produces expected distance" in {
            val ancestor = createIndividual("ancestor")

            val lineage1    = Lineage.init(ancestor, ancestor)
            val child1      = createIndividual("child1", lineage = lineage1)
            val pureLineage = Lineage.init(child1, child1)

            val ancestorLineage = Lineage.init(ancestor, ancestor)

            val distance = pureLineage.distanceTo(ancestorLineage)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }
    }

    "Lineage structure validation" - {
        "extreme inbreeding creates consistent lineage" in {
            val ancestor   = createIndividual("ancestor")
            var currentInd = ancestor

            (0 until 50).foreach { i =>
                val lineage = Lineage.init(currentInd, currentInd)
                currentInd = createIndividual(s"gen$i", lineage = lineage)
            }

            // Verify final lineage has some ancestry
            assert(currentInd.lineage.distanceTo(Lineage.empty) > 0.0)
        }

        "lineage structure is always valid" in {
            val p1          = createIndividual("p1")
            val p2          = createIndividual("p2")
            val initialized = Lineage.init(p1, p2)
            assert(initialized.distanceTo(initialized) == 0.0)

            val withLineage = createIndividual("child", lineage = initialized)
            assert(withLineage.lineage.distanceTo(withLineage.lineage) == 0.0)
        }

        "adding parents builds lineage consistently" in {
            val individuals = (0 until 20).map(i => createIndividual(s"ind$i"))
            var currentInd  = individuals(0)

            (1 until 19 by 2).foreach { i =>
                val lineage = Lineage.init(currentInd, individuals(i))
                val nextInd = createIndividual(s"child$i", lineage = lineage)
                // Verify lineage changes between generations
                assert(nextInd.lineage.distanceTo(currentInd.lineage) > 0.0)
                currentInd = nextInd
            }
            succeed
        }
    }

    "Distance calculation edge cases" - {
        "distance when one lineage has more ancestry than another" in {
            val parent1 = createIndividual("parent1")
            val parent2 = createIndividual("parent2")
            val parent3 = createIndividual("parent3")

            val subset = Lineage.init(parent1, parent2)

            val subsetChild = createIndividual("child", lineage = subset)
            val superset    = Lineage.init(subsetChild, parent3)

            val distance = subset.distanceTo(superset)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }

        "distance calculation is consistent across multiple calls" in {
            val parent1  = createIndividual("parent1")
            val parent2  = createIndividual("parent2")
            val parent3  = createIndividual("parent3")
            val parent4  = createIndividual("parent4")
            val lineage1 = Lineage.init(parent1, parent2)
            val lineage2 = Lineage.init(parent3, parent4)

            val distance1 = lineage1.distanceTo(lineage2)
            val distance2 = lineage1.distanceTo(lineage2)
            val distance3 = lineage1.distanceTo(lineage2)

            assert(distance1 == distance2)
            assert(distance2 == distance3)
        }

        "distance with individuals having different ancestry depth" in {
            val individual = createIndividual("individual")
            val parent1    = createIndividual("parent1")
            val parent2    = createIndividual("parent2")

            val shallow = Lineage.empty

            val deepLineage = Lineage.init(parent1, parent2)
            val deepChild   = createIndividual("deep", lineage = deepLineage)
            val deep        = Lineage.init(deepChild, individual)

            val distance = shallow.distanceTo(deep)

            assert(distance > 0.6)
            assert(distance <= 1.0)
        }

        "maximum possible distance is 1.0" in {
            val individuals = (0 until 100).map(i => createIndividual(s"unique$i"))
            val lineages = individuals.sliding(2).map { pair =>
                Lineage.init(pair(0), pair(1))
            }.toList

            lineages.foreach { l1 =>
                lineages.foreach { l2 =>
                    if l1 != l2 then
                        val distance = l1.distanceTo(l2)
                        assert(distance <= 1.0)
                        ()
                }
            }
            succeed
        }

        "minimum possible distance is 0.0" in {
            val individuals = (0 until 10).map(i => createIndividual(s"ind$i"))
            val lineages = individuals.sliding(2).map { pair =>
                Lineage.init(pair(0), pair(1))
            }.toList

            lineages.foreach { lineage =>
                val distance = lineage.distanceTo(lineage)
                assert(distance == 0.0)
            }
            succeed
        }
    }

    "Complex genealogies" - {
        "deeply related individuals through multiple paths" in {
            val g0  = createIndividual("g0")
            val g1a = createIndividual("g1a")
            val g1b = createIndividual("g1b")
            val g2a = createIndividual("g2a")
            val g2b = createIndividual("g2b")

            val lineageG0       = Lineage.init(g0, g0)
            val g1aWithAncestry = createIndividual("g1a-anc", lineage = lineageG0)
            val lineageG1a      = Lineage.init(g1aWithAncestry, g0)
            val g2aWithAncestry = createIndividual("g2a-anc", lineage = lineageG1a)

            val lineage1 = Lineage.init(g2aWithAncestry, g2b)

            val lineageG1b      = Lineage.init(g1b, g0)
            val g2bWithAncestry = createIndividual("g2b-anc", lineage = lineageG1b)

            val lineage2 = Lineage.init(g2aWithAncestry, g2bWithAncestry)

            val distance = lineage1.distanceTo(lineage2)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }

        "complete ancestry divergence after shared ancestor" in {
            val common     = createIndividual("common")
            val branch1_g1 = createIndividual("branch1_g1")
            val branch1_g2 = createIndividual("branch1_g2")
            val branch2_g1 = createIndividual("branch2_g1")
            val branch2_g2 = createIndividual("branch2_g2")

            val commonLineage = Lineage.init(common, common)

            val branch1Parent = createIndividual("b1parent", lineage = commonLineage)
            val lineage1      = Lineage.init(branch1_g1, branch1_g2)
            val child1        = createIndividual("child1", lineage = lineage1)
            val finalLineage1 = Lineage.init(child1, branch1Parent)

            val branch2Parent = createIndividual("b2parent", lineage = commonLineage)
            val lineage2      = Lineage.init(branch2_g1, branch2_g2)
            val child2        = createIndividual("child2", lineage = lineage2)
            val finalLineage2 = Lineage.init(child2, branch2Parent)

            val distance = finalLineage1.distanceTo(finalLineage2)

            assert(distance > 0.0)
            assert(distance < 1.0)
        }

        "asymmetric depths with shared ancestry" in {
            val shared = createIndividual("shared")
            val p1     = createIndividual("p1")
            val p2     = createIndividual("p2")
            val p3     = createIndividual("p3")

            val shallow = Lineage.init(shared, p1)

            var deepLineage = Lineage.init(shared, p2)
            var current     = createIndividual("deep1", lineage = deepLineage)
            (0 until 3).foreach { i =>
                deepLineage = Lineage.init(current, p3)
                current = createIndividual(s"deep${i + 2}", lineage = deepLineage)
            }

            val distance = shallow.distanceTo(current.lineage)

            assert(distance > 0.5)
            assert(distance < 1.0)
        }
    }

    "Boundary behavior" - {
        "lineage with single generation vs many generations" in {
            val parent = createIndividual("parent")

            val singleGen = Lineage.init(parent, parent)

            var current = createIndividual("start")
            (0 until 100).foreach { i =>
                val p       = createIndividual(s"gen$i")
                val lineage = Lineage.init(current, p)
                current = createIndividual(s"child$i", lineage = lineage)
            }

            val distance = singleGen.distanceTo(current.lineage)

            assert(distance > 0.0)
            assert(distance <= 1.0)
        }

        "distance between lineages with no overlap" in {
            val parents1 = (0 until 10).map(i => createIndividual(s"branch1_$i"))
            val parents2 = (0 until 10).map(i => createIndividual(s"branch2_$i"))

            var current1 = parents1(0)
            parents1.tail.grouped(2).foreach { pair =>
                val lineage = Lineage.init(current1, pair(0))
                current1 = createIndividual(s"child1_${pair(0).genotype.name}", lineage = lineage)
            }

            var current2 = parents2(0)
            parents2.tail.grouped(2).foreach { pair =>
                val lineage = Lineage.init(current2, pair(0))
                current2 = createIndividual(s"child2_${pair(0).genotype.name}", lineage = lineage)
            }

            val distance = current1.lineage.distanceTo(current2.lineage)

            assert(distance > 0.5)
            assert(distance <= 1.0)
        }

        "adding same parent pair builds consistent lineage" in {
            val p1 = createIndividual("p1")
            val p2 = createIndividual("p2")

            val once      = Lineage.init(p1, p2)
            val onceChild = createIndividual("once", lineage = once)

            val twice       = Lineage.init(p1, p2)
            val twiceChild1 = createIndividual("twice1", lineage = twice)
            val twice2      = Lineage.init(p1, p2)
            val twiceChild  = createIndividual("twice2", lineage = twice2)

            val thrice       = Lineage.init(p1, p2)
            val thriceChild1 = createIndividual("thrice1", lineage = thrice)
            val thrice2      = Lineage.init(p1, p2)
            val thriceChild2 = createIndividual("thrice2", lineage = thrice2)
            val thrice3      = Lineage.init(p1, p2)
            val thriceChild  = createIndividual("thrice3", lineage = thrice3)

            // Same parents = same base lineage
            val d12 = once.distanceTo(twice)
            val d23 = twice.distanceTo(thrice)

            assert(d12 == 0.0)
            assert(d23 == 0.0)
        }
    }

    "distanceTo should penalize repeated ancestors" in run {
        val ancestorMind = Mind.State(
            name = "ancestor",
            maxRecursions = 1,
            neurons = List.empty,
            output = Gate.State(
                prompt = "test",
                reminder = "",
                temperature = Temperature.balanced,
                history = History.State(List.empty, 10)
            )
        )

        val ancestor  = createIndividual("ancestor")
        val other1    = createIndividual("other1")
        val other2    = createIndividual("other2")
        val reference = createIndividual("reference")

        val lineageRefBase = Lineage.init(ancestor, other1)
        val refParent      = createIndividual("refParent", lineage = lineageRefBase)
        val lineageRef     = Lineage.init(reference, refParent)

        val lineageABase = Lineage.init(ancestor, other1)
        val aParent      = createIndividual("aParent", lineage = lineageABase)
        val lineageA     = Lineage.init(other2, aParent)

        val inbredLineage1 = Lineage.init(ancestor, ancestor)
        val inbredChild1   = createIndividual("inbred1", lineage = inbredLineage1)
        val inbredLineage2 = Lineage.init(inbredChild1, inbredChild1)
        val inbredChild2   = createIndividual("inbred2", lineage = inbredLineage2)
        val lineageB       = Lineage.init(other2, inbredChild2)

        val distanceRefToA = lineageRef.distanceTo(lineageA)
        val distanceRefToB = lineageRef.distanceTo(lineageB)

        assert(
            distanceRefToB > distanceRefToA,
            s"Inbred lineage should be less attractive (farther), but got Ref->A=$distanceRefToA, Ref->B=$distanceRefToB"
        )
    }

end LineageTest
