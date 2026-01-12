package kyo.ren.network

import kyo.*
import kyo.ai.json.Json
import kyo.ai.json.doc
import zio.schema.{Schema as ZSchema, *}

case class Input(
    from: Option[Neuron.Name],
    tasks: List[String],
    context: List[String]
) derives CanEqual

object Input:

    def apply[Task: Json](task: Task)(using Frame): Input =
        apply(List(task), List.empty[String], None)

    def apply[Task: Json, Ctx: Json](
        tasks: List[Task],
        context: List[Ctx] = List.empty[String],
        from: Option[Neuron.Name] = None
    )(using Frame): Input =
        new Input(from, tasks.map(Json.encode), context.map(Json.encode))

    def toMarkdown(input: Input, idx: Int, headingLevel: Int): String =
        val fromInfo       = input.from.map(n => s",$n").getOrElse("")
        val entryHeading   = "#" * headingLevel
        val sectionHeading = "#" * (headingLevel + 1)
        val tasks = if input.tasks.nonEmpty then
            s"$sectionHeading Tasks\n\n${input.tasks.mkString("\n")}"
        else ""
        val ctx = if input.context.nonEmpty then
            s"$sectionHeading Context\n\n${input.context.mkString("\n")}"
        else ""
        val sections = List(tasks, ctx).filter(_.nonEmpty).mkString("\n\n")
        s"$entryHeading [${idx}${fromInfo}]\n\n$sections"
    end toMarkdown

    def toMarkdown(inputs: List[Input]): String =
        if inputs.isEmpty then "## Inputs [idx,from?]\n\nEmpty"
        else
            val entries = inputs.zipWithIndex.map { (input, idx) =>
                toMarkdown(input, idx, 3)
            }.mkString("\n\n")
            s"## Inputs [idx,from?]\n\n$entries"

end Input
