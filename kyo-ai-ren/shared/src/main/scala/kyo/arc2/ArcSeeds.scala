package kyo.arc2

import kyo.*
import kyo.ren.Mind
import kyo.ren.evolution.Individual
import kyo.ren.evolution.Lineage

object ArcSeeds:
    def init(using Frame): List[Individual] < AI =
        for
            seedFiles <- Path("/Users/fwbrasil/workspace/kyo-ai/kyo-ai-ren/shared/src/main/resources/arc")
                .list("json")
                .map(_.filter(path => path.toJava.getFileName.toString.startsWith("mind-seed-")))

            seedNames = seedFiles.map { path =>
                val filename = path.toJava.getFileName.toString
                filename.stripPrefix("mind-seed-").stripSuffix(".json")
            }

            shuffledNames <- Random.shuffle(List.from(seedNames))

            individuals <- Kyo.foreachIndexed(shuffledNames) { (idx, name) =>
                Mind.loadDefault("arc", "seed", name, None).map { mind =>
                    Individual(mind, mind, List.empty, idx % 2 == 0, Lineage.empty)
                }
            }
        yield List.from(individuals)
        end for
    end init
end ArcSeeds
