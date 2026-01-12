package kyo.kb

import kyo.*
import scala.util.hashing.MurmurHash3

sealed trait Node:
    this: Product =>
    lazy val id: Node.Id[this.type] = Node.Id(hashCode)

object Node:
    case class Id[A] private[Node] (id: Int)
end Node

case class Doc(name: String, lines: Chunk[String]) extends Node:
    val content: String   = lines.mkString("\n")
    override def toString = s"Document($name)"

case class Symbol(name: String) extends Node

object Doc:
    def apply(name: String, content: String): Doc =
        new Doc(name, Chunk.from(content.linesIterator.zipWithIndex.map((line, idx) => s"${idx + 1}: $line")))

    case class Name(
        document: Doc,
        line: Int,
        columnStart: Int,
        columnEnd: Int,
        symbol: Symbol
    ) extends Node

    case class Lines(
        document: Doc,
        lineStart: Int,
        lineEnd: Int
    ) extends Node
end Doc

enum Member extends Node:
    case Type(
        name: Doc.Name,
        `package`: Doc.Name,
        scaladoc: Maybe[Doc.Lines],
        signature: Doc.Lines,
        accessible: Boolean,
        instanceMembers: Chunk[Member],
        extensionMethods: Chunk[Method],
        companionMembers: Chunk[Member]
    )

    case Method(
        name: Doc.Name,
        scaladoc: Maybe[Doc.Lines],
        signature: Doc.Lines,
        visibility: Doc.Name,
        body: Doc.Lines
    )
end Member

case class Question(
    nodes: Chunk[Node],
    question: String,
    answer: String
) extends Node

case class Test(
    label: Doc.Lines,
    body: Doc.Lines,
    children: Chunk[Test]
)
