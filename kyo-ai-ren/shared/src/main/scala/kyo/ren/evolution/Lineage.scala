package kyo.ren.evolution

import kyo.*
import kyo.ai.json.Json
import kyo.ren.*

opaque type Lineage = List[Int]

object Lineage:

    given CanEqual[Lineage, Lineage] = CanEqual.derived
    given Json[Lineage]              = Json[List[Int]]

    def empty: Lineage = List.empty

    def init(parent1: Individual, parent2: Individual): Lineage =
        val zipped = parent1.lineage.zip(parent2.lineage).flatMap(List(_, _))
        (List(parent1.genotype.hashCode, parent2.genotype.hashCode) ++
            zipped ++ parent1.lineage.drop(zipped.size) ++ parent2.lineage.drop(zipped.size)).take(10)
    end init

    extension (self: Lineage)

        def distanceTo(other: Lineage): Double =
            if self.isEmpty && other.isEmpty then 1
            else if self == other then 0
            else
                val selfSet  = self.toSet
                val otherSet = other.toSet
                val shared   = selfSet.intersect(otherSet).size
                val total    = selfSet.union(otherSet).size

                if total == 0 then 1.0
                else 1.0 - (shared.toDouble / total)
        end distanceTo
    end extension

end Lineage
