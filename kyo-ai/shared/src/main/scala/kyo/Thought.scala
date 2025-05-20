package kyo

import kyo.*
import kyo.AI
import kyo.ai.json.Json
import kyo.ai.json.description
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag
import zio.schema.FieldSet
import zio.schema.Schema.Field
import zio.schema.Schema as ZSchema
import zio.schema.TypeId
import zio.schema.validation.Validation

/** Implements structured thinking for language model generation.
  *
  * `Thought` is Kyo's structured thinking framework, a technique that embeds explicit reasoning steps into the language model generation
  * process. This technique enhances model performance by encouraging methodical reasoning and making the model's thought process
  * transparent.
  *
  * Mechanically, structured thinking works by extending the model's required output schema. Each thought becomes a field in the JSON
  * schema, forcing the model to generate explicit reasoning for that field. Thoughts are categorized as either `Opening` (executed before
  * the main generation) or `Closing` (executed after the main generation), creating a complete thinking frame around the core output.
  *
  * The technical implementation uses a type-driven approach to schema construction. Each thought is associated with a specific type `A`
  * that determines its schema structure. This type-based approach enables static verification of thought compatibility and automatic JSON
  * serialization/deserialization.
  *
  * A powerful feature of structured thinking is the ability to process thought content after generation. Each thought can include a
  * processing function that executes when the model returns that thought content. This processing capability enables:
  *
  *   - Running additional generations based on intermediate reasoning
  *   - Performing verification on reasoning steps
  *   - Emitting metrics or logging based on model reasoning
  *   - Triggering side effects or downstream processes
  *   - Building complex reasoning workflows with dependencies
  *
  * Thoughts can be composed through aggregation, allowing complex thinking patterns to be built from simpler components. The framework
  * includes default thoughts for common reasoning patterns, but custom thoughts can be defined for specific domains or tasks.
  *
  * @tparam S
  *   The capability set required by this thought
  */
sealed trait Thought[-S]:

    /** Enables this thought for a computation with additional capabilities.
      *
      * @param v
      *   The computation to run with the enabled thought
      * @return
      *   The computation with the thought enabled and combined capabilities
      */
    def enable[A, S2](v: A < S2)(using Frame): A < (S & S2) =
        Thought.enable(this)(v)

    private[kyo] def infos: Seq[Thought.internal.Info[?, S]]
end Thought

object Thought:

    import internal.*

    /** Defines the position of a thought in the generation process.
      *
      *   - Opening: Thoughts that occur before the main generation
      *   - Closing: Thoughts that occur after the main generation
      */
    enum Position derives CanEqual:
        case Opening, Closing

    /** Creates an opening thought with the specified type and no processing function.
      *
      * @tparam A
      *   The type of the thought content
      * @return
      *   A thought that will be executed before the main generation
      */
    def opening[A: Json: ClassTag]: Thought[Any] = opening[A](_ => ())

    /** Creates a closing thought with the specified type and no processing function.
      *
      * @tparam A
      *   The type of the thought content
      * @return
      *   A thought that will be executed after the main generation
      */
    def closing[A: Json: ClassTag]: Thought[Any] = closing[A](_ => ())

    /** Creates an opening thought with the specified type and processing function.
      *
      * @tparam A
      *   The type of the thought content
      * @tparam S
      *   The capability set required by the processing function
      * @param process
      *   A function that processes the thought content after generation
      * @return
      *   A thought that will be executed before the main generation
      */
    def opening[A: Json](using ClassTag[A])[S](process: A => Unit < (AI & S))(using Isolate.Contextual[S, AI]): Thought[S] =
        init[A](Position.Opening)(process)

    /** Creates a closing thought with the specified type and processing function.
      *
      * @tparam A
      *   The type of the thought content
      * @tparam S
      *   The capability set required by the processing function
      * @param process
      *   A function that processes the thought content after generation
      * @return
      *   A thought that will be executed after the main generation
      */
    def closing[A: Json](using ClassTag[A])[S](process: A => Unit < (AI & S))(using Isolate.Contextual[S, AI]): Thought[S] =
        init[A](Position.Closing)(process)

    /** Combines multiple thoughts into a single aggregate thought.
      *
      * @tparam S
      *   The combined capability set required by all thoughts
      * @param thoughts
      *   The thoughts to combine
      * @return
      *   A single thought that includes all the provided thoughts
      */
    def aggregate[S](thoughts: Thought[S]*): Thought[S] =
        new Thought[S]:
            def infos = thoughts.flatMap(_.infos)

    /** Creates a thought with the specified type, position, and processing function.
      *
      * @tparam A
      *   The type of the thought content
      * @tparam S
      *   The capability set required by the processing function
      * @param position
      *   The position of the thought (Opening or Closing)
      * @param process
      *   A function that processes the thought content after generation
      * @return
      *   A thought with the specified configuration
      */
    def init[A: Json](position: Position)[S](process: A => Unit < (AI & S))(
        using
        ct: ClassTag[A],
        isolate: Isolate.Contextual[S, AI]
    ): Thought[S] =
        new Thought[S]:
            def infos = Seq(Info(ct.runtimeClass.getSimpleName(), position, Json[A], process))

    /** Enables one or more thoughts for a computation.
      *
      * This method applies the provided thoughts to the computation, making them available in the context when the computation is executed.
      *
      * @tparam A
      *   The return type of the computation
      * @tparam S
      *   The capability set required by the computation and thoughts
      * @param thoughts
      *   The thoughts to enable
      * @param v
      *   The computation to run with the enabled thoughts
      * @return
      *   The computation with the thoughts enabled
      */
    def enable[A, S](thoughts: Thought[S]*)(v: A < S)(using Frame): A < S =
        local.update(_ ++ thoughts.asInstanceOf[Seq[Thought[Any]]])(v)

    private[kyo] object internal:

        case class Info[A, -S](
            name: String,
            position: Position,
            valueSchema: Json[A],
            process: A => Unit < (AI & S)
        )

        case class Reflect(
            `Let me reflect if I understand my role, what I need to do, and how I'll proceed`: String,
            `I'll stricly follow the tool's json schema and system instructions`: Boolean = true
        )

        case class Check(
            `I have strictly followed my role and all system instructions`: Boolean = true
        )

        val default: Thought[Any] =
            Thought.aggregate(
                Thought.opening[Reflect],
                Thought.closing[Check]
            )

        val local = Local.init(Seq.empty[Thought[Any]])

        def infos(using Frame): Seq[Info[?, ?]] < Any =
            local.use(seq => if seq.isEmpty then default.infos else seq.flatMap(_.infos))

        @description("Generate valid json strictly following the json schema. Do NOT generate xml-like content.")
        case class Result[Opening <: ListMap[String, Any], T, Closing <: ListMap[String, Any]](
            openingThoughts: Opening,
            resultValue: T,
            closingThoughts: Closing
        )

        def resultJson[A: Json](thoughts: Seq[Info[?, ?]]): Json[Result[?, A, ?]] =
            def schema[T](name: String, l: Seq[Info[?, ?]]): ZSchema[T] =
                val fields = l.map { t =>
                    import zio.schema.Schema.*
                    Field.apply[ListMap[String, Any], Any](
                        t.name,
                        t.valueSchema.zioSchema.asInstanceOf[ZSchema[Any]],
                        zio.Chunk.empty,
                        Validation.succeed,
                        identity,
                        (_, _) => ListMap.empty
                    )
                }
                val r = ZSchema.record(TypeId.fromTypeName(name), FieldSet(fields*))
                r.asInstanceOf[ZSchema[T]]
            end schema
            val (opening, closing) = thoughts.partition(_.position == Position.Opening)
            type Opening <: ListMap[String, Any]
            type Closing <: ListMap[String, Any]
            given o: ZSchema[Opening] = schema("OpeningThoughts", opening)
            given c: ZSchema[Closing] = schema("ClosingThoughts", closing)
            given t: ZSchema[A]       = Json[A].zioSchema
            Json[Result[Opening, A, Closing]].asInstanceOf[Json[Result[?, A, ?]]]
        end resultJson

        def handle[A](thoughts: Seq[Info[?, ?]], result: Result[?, A, ?])(using Frame): A < AI =
            Async.foreach((result.openingThoughts ++ result.closingThoughts).toSeq) { (name, value) =>
                thoughts.find(_.name == name) match
                    case None => AI.fail("invalid thought")
                    case Some(info) =>
                        info.asInstanceOf[Info[Any, Any]].process(value)
            }.andThen(result.resultValue)
        end handle
    end internal

end Thought
