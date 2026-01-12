package kyo.ai.json

import kyo.Frame
import scala.reflect.ClassTag
import zio.Chunk
import zio.json.*
import zio.schema.{Schema as ZSchema, *}
import zio.schema.annotation.noDiscriminator
import zio.schema.annotation.simpleEnum

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
        title: Option[String] = None,
        description: Option[String] = None,
        `enum`: Option[List[JsonStringValue]] = None,
        minLength: Option[Int] = None,
        maxLength: Option[Int] = None,
        pattern: Option[String] = None
    ) extends JsonSchema

    case class JsonNumberSchema(
        `type`: String = "number",
        title: Option[String] = None,
        description: Option[String] = None,
        `enum`: Option[List[JsonNumberValue]] = None,
        minimum: Option[BigDecimal] = None,
        maximum: Option[BigDecimal] = None,
        exclusiveMinimum: Option[BigDecimal] = None,
        exclusiveMaximum: Option[BigDecimal] = None,
        multipleOf: Option[BigDecimal] = None
    ) extends JsonSchema

    case class JsonBooleanSchema(
        `type`: String = "boolean",
        title: Option[String] = None,
        description: Option[String] = None,
        `enum`: Option[List[JsonBooleanValue]] = None
    ) extends JsonSchema

    case class JsonArraySchema(
        `type`: String = "array",
        title: Option[String] = None,
        description: Option[String] = None,
        items: JsonSchema,
        minItems: Option[Int] = None,
        maxItems: Option[Int] = None,
        uniqueItems: Option[Boolean] = None
    ) extends JsonSchema

    case class JsonObjectSchema(
        `type`: String = "object",
        title: Option[String] = None,
        description: Option[String] = None,
        properties: Map[String, JsonSchema],
        required: List[String] = Nil,
        // additionalProperties: Boolean = false,
        minProperties: Option[Int] = None,
        maxProperties: Option[Int] = None
    ) extends JsonSchema

    case class Const[T](v: T)

    def apply(schema: ZSchema[?]): JsonSchema =
        convert(schema)

    given CanEqual[StandardType[?], StandardType[?]] = CanEqual.derived

    def convert(schema: ZSchema[?]): JsonSchema =

        def annotation[A <: ValueAnnotation[?]](
            using ClassTag[A]
        )[B](using ev: A <:< ValueAnnotation[B]): Option[B] =
            schema.annotations.collectFirst {
                case ann: A => ev(ann).value
            }

        def applyFieldAnnotations(baseSchema: JsonSchema, fieldAnnotations: Chunk[Any]): JsonSchema =
            def fieldAnnotation[A <: ValueAnnotation[?]](
                using ClassTag[A]
            )[B](using ev: A <:< ValueAnnotation[B]): Option[B] =
                fieldAnnotations.collectFirst {
                    case ann: A => ev(ann).value
                }

            baseSchema match
                case s: JsonStringSchema => s.copy(
                        description = fieldAnnotation[doc].orElse(s.description),
                        title = fieldAnnotation[title].orElse(s.title),
                        minLength = fieldAnnotation[minLength].orElse(s.minLength),
                        maxLength = fieldAnnotation[maxLength].orElse(s.maxLength),
                        pattern = fieldAnnotation[pattern].orElse(s.pattern)
                    )
                case s: JsonNumberSchema => s.copy(
                        description = fieldAnnotation[doc].orElse(s.description),
                        title = fieldAnnotation[title].orElse(s.title),
                        minimum = fieldAnnotation[minimum].orElse(s.minimum),
                        maximum = fieldAnnotation[maximum].orElse(s.maximum),
                        exclusiveMinimum = fieldAnnotation[exclusiveMinimum].orElse(s.exclusiveMinimum),
                        exclusiveMaximum = fieldAnnotation[exclusiveMaximum].orElse(s.exclusiveMaximum),
                        multipleOf = fieldAnnotation[multipleOf].orElse(s.multipleOf)
                    )
                case s: JsonBooleanSchema => s.copy(
                        description = fieldAnnotation[doc].orElse(s.description),
                        title = fieldAnnotation[title].orElse(s.title)
                    )
                case s: JsonArraySchema => s.copy(
                        description = fieldAnnotation[doc].orElse(s.description),
                        title = fieldAnnotation[title].orElse(s.title),
                        minItems = fieldAnnotation[minItems].orElse(s.minItems),
                        maxItems = fieldAnnotation[maxItems].orElse(s.maxItems),
                        uniqueItems = fieldAnnotation[uniqueItems].orElse(s.uniqueItems)
                    )
                case s: JsonObjectSchema => s.copy(
                        description = fieldAnnotation[doc].orElse(s.description),
                        title = fieldAnnotation[title].orElse(s.title),
                        minProperties = fieldAnnotation[minProperties].orElse(s.minProperties),
                        maxProperties = fieldAnnotation[maxProperties].orElse(s.maxProperties)
                    )
            end match
        end applyFieldAnnotations

        ZSchema.force(schema) match
            case ZSchema.Primitive(StandardType.StringType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[String] }.get

                JsonStringSchema(
                    `enum` = Some(List(JsonStringValue(constValue))),
                    description = annotation[doc],
                    title = annotation[title],
                    minLength = annotation[minLength],
                    maxLength = annotation[maxLength],
                    pattern = annotation[pattern]
                )

            case ZSchema.Primitive(StandardType.StringType, _) =>
                JsonStringSchema(
                    description = annotation[doc],
                    title = annotation[title],
                    minLength = annotation[minLength],
                    maxLength = annotation[maxLength],
                    pattern = annotation[pattern]
                )

            case ZSchema.Primitive(StandardType.IntType, chunk) if chunk.exists(_.isInstanceOf[Const[?]]) =>
                val constValue = chunk.collectFirst { case Const(v) => v.asInstanceOf[Int] }.get

                JsonNumberSchema(
                    `enum` = Some(List(JsonNumberValue(constValue))),
                    description = annotation[doc],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.IntType, _) =>
                JsonNumberSchema(
                    description = annotation[doc],
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
                    description = annotation[doc],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.LongType, _) =>
                JsonNumberSchema(
                    description = annotation[doc],
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
                    description = annotation[doc],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.DoubleType, _) =>
                JsonNumberSchema(
                    description = annotation[doc],
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
                    description = annotation[doc],
                    title = annotation[title],
                    minimum = annotation[minimum],
                    maximum = annotation[maximum],
                    exclusiveMinimum = annotation[exclusiveMinimum],
                    exclusiveMaximum = annotation[exclusiveMaximum],
                    multipleOf = annotation[multipleOf]
                )

            case ZSchema.Primitive(StandardType.FloatType, _) =>
                JsonNumberSchema(
                    description = annotation[doc],
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
                    description = annotation[doc],
                    title = annotation[title]
                )

            case ZSchema.Primitive(StandardType.BoolType, _) =>
                JsonBooleanSchema(
                    description = annotation[doc],
                    title = annotation[title]
                )

            case ZSchema.Optional(innerSchema, _) =>
                convert(innerSchema)

            case ZSchema.Sequence(innerSchema, _, _, _, _) =>
                JsonArraySchema(
                    items = convert(innerSchema),
                    description = annotation[doc],
                    title = annotation[title],
                    minItems = annotation[minItems],
                    maxItems = annotation[maxItems],
                    uniqueItems = annotation[uniqueItems]
                )

            case ZSchema.Map(keySchema, valueSchema, _) =>
                JsonArraySchema(
                    items = JsonObjectSchema(
                        properties = Map(
                            "key"   -> convert(keySchema),
                            "value" -> convert(valueSchema)
                        ),
                        required = List("key", "value")
                    ),
                    description = annotation[doc],
                    title = annotation[title],
                    minItems = annotation[minItems],
                    maxItems = annotation[maxItems]
                )

            case schema: ZSchema.Enum[?] =>
                JsonObjectSchema(
                    properties = schema.cases.map(c => c.id -> convert(c.schema)).toMap,
                    description = annotation[doc],
                    title = annotation[title],
                    minProperties = annotation[minProperties],
                    maxProperties = annotation[maxProperties]
                )

            case schema: ZSchema.Record[?] =>
                val properties = schema.fields.map { field =>
                    field.name -> applyFieldAnnotations(convert(field.schema), field.annotations)
                }.toMap

                val requiredFields = schema.fields.collect {
                    case field if !isOptional(field.schema) =>
                        field.name
                }.toList

                JsonObjectSchema(
                    properties = properties.toMap,
                    required = requiredFields,
                    description = annotation[doc],
                    title = annotation[title],
                    minProperties = annotation[minProperties],
                    maxProperties = annotation[maxProperties]
                )

            case ZSchema.Transform(schema, f, g, ann, id) =>
                convert(schema)

            case schema =>
                throw new UnsupportedOperationException(
                    "This schema type is not supported: " + schema
                )
        end match
    end convert

    private def isOptional(schema: ZSchema[?]): Boolean =
        ZSchema.force(schema) match
            case _: ZSchema.Optional[?]               => true
            case ZSchema.Transform(inner, _, _, _, _) => isOptional(inner)
            case _                                    => false
end JsonSchema
