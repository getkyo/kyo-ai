// package kyo.kb

// import kyo.*
// import kyo.ai.Config
// import scala.collection.immutable.ListMap

// given Frame = Frame.internal

// object RawMembers:

//     import internal.*

//     def apply(doc: Doc): Chunk[RawMember] < AI =
//         AI.gen[List[ExtractMember]](doc.content).map { list =>
//             Loop(list.reverse, ListMap.empty[Int, RawMember]) {
//                 case (raw :: rest, acc) =>
//                     Loop.continue(rest, acc.updated(raw.id, raw.toRawMember(doc, acc)))
//                 case (Nil, acc) =>
//                     Loop.done(acc.values.toList.reverse)
//             }
//         }
//             .map(Chunk.from)
//             .handle(
//                 Retry(Schedule.repeat(3)),
//                 prompt.enable,
//                 Thought.closing[Validate].enable
//             )
//     end apply

//     private object internal:

//         sealed trait ExtractMember:
//             def id: Int
//             def toRawMember(doc: Doc, map: Map[Int, RawMember]): RawMember

//         object ExtractMember:
//             case class Type(
//                 id: Int,
//                 `package`: String,
//                 name: String,
//                 scaladoc: Maybe[Lines],
//                 signature: Lines,
//                 isPublic: Boolean,
//                 body: Lines,
//                 instanceMemberIds: Chunk[Int],
//                 companionMemberIds: Chunk[Int]
//             ) extends ExtractMember:
//                 def toRawMember(doc: Doc, map: Map[Int, RawMember]) =
//                     RawMember.Type(
//                         name,
//                         `package`,
//                         scaladoc.map(s => Doc.Ref(doc, s.lineStart, s.lineEnd)),
//                         Doc.Ref(doc, signature.lineStart, signature.lineEnd),
//                         isPublic,
//                         instanceMemberIds.map(map(_)),
//                         companionMemberIds.map(map(_))
//                     )
//             end Type

//             case class Method(
//                 id: Int,
//                 name: String,
//                 scaladoc: Maybe[Lines],
//                 signature: Lines,
//                 isPublic: Boolean,
//                 body: Lines
//             ) extends ExtractMember:
//                 def toRawMember(doc: Doc, map: Map[Int, RawMember]) =
//                     RawMember.Method(
//                         name,
//                         scaladoc.map(s => Doc.Ref(doc, s.lineStart, s.lineEnd)),
//                         Doc.Ref(doc, signature.lineStart, signature.lineEnd),
//                         isPublic,
//                         Doc.Ref(doc, body.lineStart, body.lineEnd)
//                     )
//             end Method
//         end ExtractMember

//         case class Lines(lineStart: Int, lineEnd: Int)

//         val prompt = Prompt.init(
//             p"""
//                 Extract a comprehensive data structure faithfully representing the source code according to the following schema:

//                 1. For each Type and Method, include ALL specified fields in the schema exactly as named.
//                 2. Pay special attention to member IDs and ensure ALL referenced IDs exist before referencing them.
//                 3. For Method entities, verify that:
//                 - id: must be a unique integer
//                 - name: the method name as a string
//                 - scaladoc: object with lineStart and lineEnd if documentation exists; if no scaladoc exists, still include the field with accurate line numbers reflecting where documentation would be
//                 - signature: object with lineStart and lineEnd of the method signature
//                 - isPublic: boolean indicating public accessibility
//                 - body: object with lineStart and lineEnd of the method implementation

//                 4. For Type entities, verify that:
//                 - id: must be a unique integer
//                 - name: the type name as a string
//                 - scaladoc: object with lineStart and lineEnd of documentation if it exists; if no scaladoc exists, still include the field with accurate line numbers reflecting where documentation would be
//                 - signature: object with lineStart and lineEnd of the type definition
//                 - isPublic: boolean indicating public accessibility
//                 - body: object with lineStart and lineEnd of the implementation
//                 - instanceMemberIds: array of integer IDs that MUST already be defined in your extraction
//                 - companionMemberIds: array of integer IDs that MUST already be defined in your extraction

//                 5. Place extension methods in the type itself instead of the companion object.
//                 6. CRITICAL: Before including any ID in instanceMemberIds or companionMemberIds, verify it exists in your defined methods.
//                 7. If you reference any ID, it MUST be present in your result set.
//                 8. Line numbers in the source file start at line 1 (not 0).
//                 9. BEFORE returning your result, validate that all referenced IDs exist to prevent NoSuchElementException.
//                 10. Ensure every method or type ID you extract is included in your result set before being referenced by another entity.
//             """,
//             p"""
//                 FINAL REMINDERS:

//                 1. VALIDATE ALL IDs: Every ID referenced in instanceMemberIds or companionMemberIds must exist in your result set
//                 2. COMPLETE SCHEMA: Include all required fields for each Type and Method
//                 3. PROPER SCALADOC: For methods/types without documentation, still include the scaladoc field with appropriate line positions
//                 4. METHOD PLACEMENT: Place extension methods in the type itself, not in companion objects
//                 5. LINE NUMBERING: Line numbers start at 1, not 0
//                 6. ID VERIFICATION: Double-check your result to ensure no referenced IDs are missing
//                 7. CONSISTENCY CHECK: Verify that all member IDs in your output are consistent and properly defined before being referenced

//                 Before submitting, scan your entire output to catch any potential NoSuchElementException scenarios.
//             """
//         )

//         case class Elaborate(
//             `Let me elaborate how I'll make sure IDs and lines are correct`: String
//         )

//         case class Validate(
//             `I have validated that every referenced ID  exists in my result set`: String,
//             `I have placed all extension methods in the type itself rather than the companion object`: String,
//             `I have included the proper line numbers for all elements (starting from line 1, not 0)`: String,
//             `I have double-checked that all scaladoc fields are properly included with appropriate line numbers`: String,
//             `I have ensured my extraction is complete with all Types and Methods fully represented`: String
//         )
//     end internal
// end RawMembers
