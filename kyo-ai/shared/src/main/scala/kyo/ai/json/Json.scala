package kyo.ai.json

import kyo.*
import zio.json.*
import zio.schema.{Schema as ZSchema, *}
import zio.schema.codec.JsonCodec

trait Json[T]:
    def jsonSchema: JsonSchema
    def encode(v: T)(using Frame): String
    def encodePretty(v: T)(using Frame): String
    def decode(s: String)(using Frame): Result[AIException, T]
    def zioSchema: ZSchema[T]
    def zioDecoder: JsonDecoder[T]
    def zioEncoder: JsonEncoder[T]
end Json

object Json extends JsonDerive:

    given [A: Json as json]: Json[Maybe[A]] =
        fromZio(json.zioSchema.optional.transform(Maybe.fromOption, _.toOption))

    given [A: Json as json]: Json[Chunk[A]] =
        given ZSchema[A] = json.zioSchema
        fromZio(ZSchema[List[A]].transform(Chunk.from(_), _.toList))

    given Json[Duration] = fromZio(ZSchema[Long].transform(Duration.fromNanos, _.toNanos))

    def apply[T](using j: Json[T]): Json[T] =
        j

    def jsonSchema[T](using j: Json[T]): JsonSchema =
        j.jsonSchema

    def encode[T](v: T)(using j: Json[T], f: Frame): String =
        j.encode(v)

    def encodePretty[T](v: T)(using j: Json[T], f: Frame): String =
        j.encodePretty(v)

    def decode[T](s: String)(using j: Json[T], f: Frame): Result[AIException, T] =
        j.decode(s)

    given primitive[T](using t: StandardType[T]): Json[T] =
        fromZio(ZSchema.Primitive(t, zio.Chunk.empty))

    def fromZio[T](z: ZSchema[T]) =
        new Json[T]:
            given zioSchema: ZSchema[T]      = z
            lazy val jsonSchema              = JsonSchema(z)
            given zioDecoder: JsonDecoder[T] = JsonCodec.jsonDecoder(z)
            given zioEncoder: JsonEncoder[T] = JsonCodec.jsonEncoder(z)

            def encode(v: T)(using Frame): String =
                v.toJson

            def encodePretty(v: T)(using Frame): String =
                v.toJsonPretty

            def decode(s: String)(using Frame): Result[AIException, T] =
                zioDecoder.decodeJson(s) match
                    case Left(error) => Result.fail(AIException(error))
                    case Right(v)    => Result.succeed(v)
end Json
