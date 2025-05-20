package kyo.ai.json

import kyo.Frame
import scala.reflect.ClassTag
import zio.Chunk
import zio.json.*
import zio.schema.{Schema as ZSchema, *}
import zio.schema.annotation.noDiscriminator

@noDiscriminator
sealed trait JsonSchema:
    def `type`: String

object JsonSchema:

    sealed trait JsonValue
    case class JsonStringValue(value: String)     extends JsonValue
    case class JsonNumberValue(value: BigDecimal) extends JsonValue
    case class JsonBooleanValue(value: Boolean)   extends JsonValue

    case class JsonStringSchema(
        `type`: String = "string",
        `enum`: Option[List[JsonStringValue]] = None,
        description: Option[String] = None,
        title: Option[String] = None,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        pattern: Option[String] = None
    ) extends JsonSchema

    case class JsonNumberSchema(
        `type`: String = "number",
        `enum`: Option[List[JsonNumberValue]] = None,
        description: Option[String] = None,
        title: Option[String] = None,
        minimum: Option[BigDecimal] = None,
        maximum: Option[BigDecimal] = None,
        exclusiveMinimum: Option[BigDecimal] = None,
        exclusiveMaximum: Option[BigDecimal] = None,
        multipleOf: Option[BigDecimal] = None
    ) extends JsonSchema

    case class JsonBooleanSchema(
        `type`: String = "boolean",
        `enum`: Option[List[JsonBooleanValue]] = None,
        description: Option[String] = None,
        title: Option[String] = None
    ) extends JsonSchema

    case class JsonArraySchema(
        items: JsonSchema,
        description: Option[String] = None,
        title: Option[String] = None,
        minItems: Option[Int] = None,
        maxItems: Option[Int] = None,
        uniqueItems: Option[Boolean] = None,
        `type`: String = "array"
    ) extends JsonSchema

    case class JsonObjectSchema(
        `type`: String = "object",
        properties: Map[String, JsonSchema],
        required: List[String] = Nil,
        additionalProperties: Boolean = false,
        description: Option[String] = None,
        title: Option[String] = None,
        minProperties: Option[Int] = None,
        maxProperties: Option[Int] = None
    ) extends JsonSchema

    case class Const[T](v: T)

    def apply(schema: ZSchema[?]): JsonSchema =
        convert(schema)

    given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

    def convert(schema: ZSchema[?]): JsonSchema = convert(schema, schema.annotations)

    def convert(schema: ZSchema[?], annotations: Chunk[Any]): JsonSchema =

        def annotation[A <: ValueAnnotation[?]](
            using ClassTag[A]
        )[B](using ev: A <:< ValueAnnotation[B]): Option[B] =
            annotations.collectFirst {
                case ann: A => ev(ann).value
            }

        ZSchema.force(schema) match
            case ZSchema.Primitive(StandardType.StringType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[String] }.get

                JsonStringSchema(
                    `enum` = Some(List(JsonStringValue(constValue))),
                    description = annotation[description],
                    title = annotation[title],
                    minLength = annotation[minLength],
                    maxLength = annotation[maxLength],
                    pattern = annotation[pattern]
                )

            case ZSchema.Primitive(StandardType.StringType, _) =>
                JsonStringSchema(
                    description = annotation[description],
                    title = annotation[title],
                    minLength = annotation[minLength],
                    maxLength = annotation[maxLength],
                    pattern = annotation[pattern]
                )

            case ZSchema.Primitive(StandardType.IntType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[Int] }.get

                JsonNumberSchema(
                    `enum` = Some(List(JsonNumberValue(constValue))),
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.IntType, _) =>
                JsonNumberSchema(
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.LongType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[Long] }.get

                JsonNumberSchema(
                    `enum` = Some(List(JsonNumberValue(constValue))),
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.LongType, _) =>
                JsonNumberSchema(
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.DoubleType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[Double] }.get

                JsonNumberSchema(
                    `enum` = Some(List(JsonNumberValue(constValue))),
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.DoubleType, _) =>
                JsonNumberSchema(
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.FloatType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[Float] }.get

                JsonNumberSchema(
                    `enum` = Some(List(JsonNumberValue(constValue.toDouble))),
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.FloatType, _) =>
                JsonNumberSchema(
                    description = annotation[description],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.BoolType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[Boolean] }.get
                JsonBooleanSchema(
                    `enum` = Some(List(JsonBooleanValue(constValue))),
                    description = annotation[description],
                    title = annotation[title]
                )

            case ZSchema.Primitive(StandardType.BoolType, _) =>
                JsonBooleanSchema(
                    description = annotation[description],
                    title = annotation[title]
                )

            case ZSchema.Optional(innerSchema, _) =>
                convert(innerSchema)

            case ZSchema.Sequence(innerSchema, _, _, _, _) =>
                JsonArraySchema(
                    items = convert(innerSchema),
                    description = annotation[description],
                    title = annotation[title],
                    minItems = annotation[minItems],
                    maxItems = annotation[maxItems],
                    uniqueItems = annotation[uniqueItems]
                )

            case schema: ZSchema.Enum[?] =>
                def handle(s: ZSchema[?]): JsonSchema =
                    ZSchema.force(s) match
                        case record: ZSchema.Record[?] =>
                            val fields = record.fields.map { field =>
                                field.name -> convert(field.schema)
                            }.toMap

                            val requiredFields = record.fields.collect {
                                case field if !field.schema.isInstanceOf[ZSchema.Optional[?]] =>
                                    field.name
                            }.toList

                            JsonObjectSchema(
                                properties = fields.toMap,
                                required = requiredFields
                            )

                        case ZSchema.Transform(innerSchema, _, _, _, _) =>
                            handle(innerSchema)

                        case s =>
                            throw new UnsupportedOperationException("Non-record enum case is not supported: " + s)

                val cases = schema.cases.map(c => c.id -> handle(c.schema)).toMap

                JsonObjectSchema(
                    properties = cases,
                    description = annotation[description],
                    title = annotation[title],
                    minProperties = annotation[minProperties],
                    maxProperties = annotation[maxProperties]
                )

            case schema: ZSchema.Record[?] =>
                val properties = schema.fields.map { field =>
                    field.name -> convert(field.schema, field.annotations)
                }.toMap

                val requiredFields = schema.fields.collect {
                    case field if !field.schema.isInstanceOf[ZSchema.Optional[?]] =>
                        field.name
                }.toList

                JsonObjectSchema(
                    properties = properties.toMap,
                    required = requiredFields,
                    description = annotation[description],
                    title = annotation[title],
                    minProperties = annotation[minProperties],
                    maxProperties = annotation[maxProperties]
                )

            // case ZSchema.Map(keySchema, valueSchema, _) =>
            //     ZSchema.force(keySchema) match
            //         case ZSchema.Primitive(tpe, _) if tpe == StandardType.StringType =>
            //             JsonObjectSchema(
            //                 properties = Map.empty,
            //                 additionalProperties = Some(convert(valueSchema)),
            //                 description = annotation[description],
            //                 title = annotation[title],
            //                 minProperties = annotation[minProperties],
            //                 maxProperties = annotation[maxProperties]
            //             )
            //         case _ =>
            //             throw new UnsupportedOperationException("Non-string map keys are not supported")

            case ZSchema.Transform(schema, f, g, ann, id) =>
                convert(schema)

            case schema =>
                throw new UnsupportedOperationException(
                    "This schema type is not supported: " + schema
                )
        end match
    end convert
end JsonSchema
