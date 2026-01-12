package kyo.ren.network

import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc

final class History(
    init: History.State
):
    private var entries = init.entries

    def apply[Out: Json](inputs: List[Input], output: Out)(using Frame): Unit < Sync =
        History.nextId.incrementAndGet.map { id =>
            entries =
                (entries :+ History.Entry(id, inputs, Json.encode(output)))
                    .takeRight(init.maxEntries)
        }

    def state(using Frame): History.State < Sync =
        Sync.defer {
            init.copy(entries = entries)
        }
end History

object History:

    private val nextId =
        import AllowUnsafe.embrace.danger
        AtomicInt.Unsafe.init.safe

    case class Entry(
        id: Int,
        inputs: List[Input],
        output: String
    ) derives CanEqual, Json

    case class State(
        entries: List[Entry],
        maxEntries: Int
    ) derives CanEqual, Json

    def init(state: State)(using Frame): History < Sync =
        Sync.defer(History(state))

    def toMarkdown(entries: List[Entry]): String =
        if entries.isEmpty then "## History [id]\n\nEmpty"
        else
            val logEntries = entries.map { entry =>
                val inputsSection = if entry.inputs.nonEmpty then
                    val inputsList = entry.inputs.zipWithIndex.map { (input, idx) =>
                        Input.toMarkdown(input, idx, 5)
                    }.mkString("\n\n")
                    s"#### Inputs\n\n$inputsList"
                else ""
                val outputSection = s"#### Output\n\n${entry.output}"
                val sections      = List(inputsSection, outputSection).filter(_.nonEmpty).mkString("\n\n")
                s"### [${entry.id}]\n\n$sections"
            }.mkString("\n\n")
            s"## History [id]\n\n$logEntries"

end History
