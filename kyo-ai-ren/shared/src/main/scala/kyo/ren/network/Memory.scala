package kyo.ren.network

import Memory.*
import kyo.*
import kyo.ai.json.*
import kyo.ai.json.doc

final class Memory(
    init: Memory.State,
    gate: Gate
):
    private var entries = init.entries

    def apply(inputs: List[Input])(using Frame): List[Memory.Entry] < AI =
        for
            entries <- Sync.defer(entries)
            memoryOutput <- gate[MemoryOutput](
                inputs :+ Input(
                    List("Consider the current mememory entries"),
                    List(Memory.toMarkdown(entries))
                )
            )
            retrievedIndices =
                memoryOutput.retrieveRanges.getOrElse(List.empty).flatMap(r => r.first to r.last)
                    .filter(idx => idx >= 0 && idx < entries.length)
            updatedWithCounts =
                entries.zipWithIndex.map { case (entry, idx) =>
                    if retrievedIndices.contains(idx) then
                        entry.copy(reads = entry.reads + 1)
                    else
                        entry
                }
            updatedEntries <- applyOps(updatedWithCounts, memoryOutput.updates.getOrElse(List.empty))
            _              <- Sync.defer { this.entries = updatedEntries }
        yield retrievedIndices.map(updatedWithCounts(_))

    private def applyOps(entries: List[Entry], ops: List[Update])(using Frame): List[Entry] < Sync =
        if entries.isEmpty then
            val adds = ops.collect { case Update.Add(_, content) => content }
            List.from(adds.map(content => Entry(0, 0, content)))
        else
            val removedIndices = ops.collect { case Update.Remove(indices) => indices }.flatten.toSet
            val replaces       = ops.collect { case Update.Replace(index, content) => (index, content) }.toMap

            val addsByIndex = ops
                .collect {
                    case Update.Add(afterIndex, content) =>
                        val lastIdx = entries.size - 1
                        (afterIndex.map(_.max(0).min(lastIdx)).getOrElse(lastIdx), content)
                }
                .groupBy(_._1)
                .map { case (idx, adds) => (idx, adds.map(_._2)) }

            val transformed =
                entries.zipWithIndex.flatMap { case (entry, originalIdx) =>
                    if replaces.contains(originalIdx) then
                        List((
                            Entry(
                                entry.reads,
                                entry.updates + 1,
                                replaces(originalIdx)
                            ),
                            originalIdx
                        ))
                    else if removedIndices.contains(originalIdx) then
                        List.empty
                    else
                        List((entry, originalIdx))
                }

            transformed.flatMap { case (entry, originalIdx) =>
                val addsAfter =
                    addsByIndex
                        .getOrElse(originalIdx, List.empty)
                        .map(content => Entry(0, 0, content))

                List(entry) ++ List.from(addsAfter)
            }
        end if
    end applyOps

    def state(using Frame): Memory.State < Sync =
        gate.state.map(Memory.State(entries, _))

end Memory

object Memory:

    case class Entry(
        reads: Int,
        updates: Int,
        content: String
    ) derives CanEqual, Json

    case class State(
        entries: List[Entry],
        gate: Gate.State
    ) derives CanEqual, Json

    def init(state: State)(using Frame): Memory < Sync =
        Gate.init(state.gate).map(Memory(state, _))

    enum Update derives CanEqual:
        case Remove(
            @doc("Indices from original state. Replace overrides Remove for same index.")
            indices: List[Int]
        )
        case Replace(
            @doc("Index from original state.")
            index: Int,
            content: String
        )
        case Add(
            @doc("Index from original state. Appends to the end if empty.")
            afterIndex: Option[Int],
            content: String
        )
    end Update

    case class Range(
        @doc("Range start")
        first: Int,
        @doc("Range end")
        last: Int
    )

    case class MemoryOutput(
        @doc("All indices refer to original state. Order in array doesn't matter. " +
            "Processed by type: 1) Remove 2) Replace 3) Add. If no updates, use an empty collection.")
        updates: Option[List[Update]],
        @doc("Ranges (inclusive) of relevant entries to retrieve for processing the input. **Use ranges, not absolute values!**")
        retrieveRanges: Option[List[Range]]
    ) derives CanEqual

    def toMarkdown(entries: List[Entry]): String =
        if entries.isEmpty then "## Memory [idx,reads,updates]\n\nEmpty"
        else
            val logEntries = entries.zipWithIndex.map { (entry, idx) =>
                s"### [${idx},${entry.reads},${entry.updates}]\n\n${entry.content}"
            }.mkString("\n\n")
            s"## Memory entries [idx,reads,updates]\n\n$logEntries"

end Memory
