package kyo.ai.json

import kyo.*
import kyo.internal.Inliner
import kyo.internal.TypeIntersection
import scala.collection.immutable.ListMap
import scala.compiletime.*
import zio.schema.{Schema as ZSchema, *}
import zio.schema.Schema.Field
import zio.schema.validation.Validation

trait JsonDerive:

    inline given derived[T]: Json[T] =
        given toZSchema[T: Json as json]: ZSchema[T] = json.zioSchema
        Json.fromZio(DeriveSchema.gen)

    inline given constString[T <: String & Singleton]: Json[T] =
        const(StandardType.StringType, compiletime.constValue[T])

    inline given constInt[T <: Int & Singleton]: Json[T] =
        const(StandardType.IntType, compiletime.constValue[T])

    inline given constLong[T <: Long & Singleton]: Json[T] =
        const(StandardType.LongType, compiletime.constValue[T])

    inline given constDoubleZSchema[T <: Double & Singleton]: Json[T] =
        const(StandardType.DoubleType, compiletime.constValue[T])

    inline given constFloatZSchema[T <: Float & Singleton]: Json[T] =
        const(StandardType.FloatType, compiletime.constValue[T])

    inline given constBoolZSchema[T <: Boolean & Singleton]: Json[T] =
        const(StandardType.BoolType, compiletime.constValue[T])

    private def const[T](t: StandardType[?], v: Any): Json[T] =
        Json.fromZio(ZSchema.Primitive(t, zio.Chunk(JsonSchema.Const(v))).asInstanceOf[ZSchema[T]])

end JsonDerive
