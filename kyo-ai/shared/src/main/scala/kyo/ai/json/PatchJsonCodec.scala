package zio.schema.codec

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import java.util
import java.util.concurrent.ConcurrentHashMap
import scala.annotation.*
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.control.NonFatal
import zio.Cause
import zio.Chunk
import zio.ChunkBuilder
import zio.ZIO
import zio.ZNothing
import zio.json.JsonCodec as ZJsonCodec
import zio.json.JsonDecoder.JsonError
import zio.json.JsonDecoder.UnsafeJson
import zio.json.JsonDecoder as ZJsonDecoder
import zio.json.JsonEncoder as ZJsonEncoder
import zio.json.JsonFieldDecoder
import zio.json.JsonFieldEncoder
import zio.json.ast.Json
import zio.json.internal.FastStringReader
import zio.json.internal.Lexer
import zio.json.internal.RecordingReader
import zio.json.internal.RetractReader
import zio.json.internal.StringMatrix
import zio.json.internal.Write
import zio.prelude.NonEmptyMap
import zio.schema.*
import zio.schema.Schema.GenericRecord
import zio.schema.annotation.*
import zio.schema.annotation.rejectExtraFields
import zio.schema.codec.JsonCodec.JsonDecoder.schemaDecoder
import zio.stream.ZChannel
import zio.stream.ZPipeline

@nowarn
object JsonCodec:

    given [A, B]: CanEqual[A, B] = CanEqual.derived

    private val streamEncoderSeparator: Chunk[Byte] = Chunk.single('\n'.toByte)

    @deprecated(
        """Use JsonCodec.Configuration instead.
JsonCodec.Configuration makes it now possible to configure en-/decoding of empty collection and nulls (Options) independently.""",
        "1.6.7"
    )
    final case class Config(
        ignoreEmptyCollections: Boolean,
        treatStreamsAsArrays: Boolean = false,
        explicitNulls: Boolean = false
    ):
        private[codec] def toConfiguration: Configuration =
            Configuration(
                explicitEmptyCollections = ExplicitConfig(
                    encoding = !ignoreEmptyCollections,
                    decoding = !ignoreEmptyCollections
                ),
                treatStreamsAsArrays = treatStreamsAsArrays,
                explicitNulls = ExplicitConfig(
                    encoding = explicitNulls,
                    decoding = explicitNulls
                )
            )
        private[codec] def this(ignoreEmptyCollections: Boolean) = this(
            ignoreEmptyCollections,
            treatStreamsAsArrays = false,
            explicitNulls = false
        )
        end this

        private[codec] def copy(
            ignoreEmptyCollections: Boolean = false,
            treatStreamsAsArrays: Boolean = false,
            explicitNulls: Boolean = false
        ): Config = new Config(
            ignoreEmptyCollections,
            treatStreamsAsArrays,
            explicitNulls
        )

        private[codec] def copy(ignoreEmptyCollections: Boolean): Config = new Config(
            ignoreEmptyCollections,
            this.treatStreamsAsArrays,
            this.explicitNulls
        )

        def copy(ignoreEmptyCollections: Boolean, treatStreamsAsArrays: Boolean): Config = new Config(
            ignoreEmptyCollections,
            treatStreamsAsArrays,
            this.explicitNulls
        )
    end Config

    @deprecated(
        """Use JsonCodec.Configuration instead.
JsonCodec.Configuration makes it now possible to configure en-/decoding of empty collection and nulls (Options) independently.""",
        "1.6.7"
    )
    object Config:

        @deprecated(
            """Use JsonCodec.Configuration instead.
JsonCodec.Configuration makes it now possible to configure en-/decoding of empty collection and nulls (Options) independently.""",
            "1.6.7"
        )
        def apply(ignoreEmptyCollections: Boolean): Config = new Config(
            ignoreEmptyCollections
        )

        val default: Config = Config(ignoreEmptyCollections = false)
    end Config

    /** When disabled for encoding, matching fields will be omitted from the JSON. When disabled for decoding, missing fields will be
      * decoded as the default value.
      */
    case class ExplicitConfig(encoding: Boolean = true, decoding: Boolean = false)

    /** Configuration for the JSON codec. The configurations are overruled by the annotations that configure the same behavior.
      *
      * @param explicitEmptyCollections
      *   whether to encode empty collections as `[]` or omit the field and decode the field when it is missing as an empty collection or
      *   fail
      * @param explicitNulls
      *   whether to encode empty Options as `null` or omit the field and decode the field when it is missing to None or fail
      * @param discriminatorSettings
      *   set up how to handle discriminators
      * @param fieldNameFormat
      *   format for the field names
      * @param treatStreamsAsArrays
      *   whether to treat streams as arrays when encoding/decoding
      * @param rejectExtraFields
      *   whether to reject extra fields during decoding
      */
    final case class Configuration(
        explicitEmptyCollections: ExplicitConfig = ExplicitConfig(),
        explicitNulls: ExplicitConfig = ExplicitConfig(),
        discriminatorSettings: DiscriminatorSetting = DiscriminatorSetting.default,
        fieldNameFormat: NameFormat = NameFormat.Identity,
        treatStreamsAsArrays: Boolean = false,
        rejectExtraFields: Boolean = false
    ):

        val noDiscriminator: Boolean = discriminatorSettings match
            case DiscriminatorSetting.NoDiscriminator => true
            case _                                    => false

        val discriminatorName: Option[String] = discriminatorSettings match
            case DiscriminatorSetting.Name(name, _) => Some(name)
            case _                                  => None

        val discriminatorFormat: NameFormat = discriminatorSettings match
            case DiscriminatorSetting.ClassName(format) => format
            case DiscriminatorSetting.Name(_, format)   => format
            case _                                      => NameFormat.Identity
    end Configuration

    object Configuration:
        val default: Configuration = Configuration()

    sealed trait DiscriminatorSetting

    object DiscriminatorSetting:
        val default: ClassName = ClassName(NameFormat.Identity)
        case class ClassName(format: NameFormat)                                extends DiscriminatorSetting
        case object NoDiscriminator                                             extends DiscriminatorSetting
        case class Name(name: String, format: NameFormat = NameFormat.Identity) extends DiscriminatorSetting
    end DiscriminatorSetting

    type DiscriminatorTuple = Option[(String, String)]

    implicit def zioJsonBinaryCodec[A](implicit jsonCodec: ZJsonCodec[A]): BinaryCodec[A] =
        new BinaryCodec[A]:
            override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
                jsonCodec
                    .decodeJson(new String(whole.toArray, JsonEncoder.CHARSET))
                    .left
                    .map(failure => DecodeError.ReadError(Cause.empty, failure))

            override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
                ZPipeline.fromChannel(
                    ZPipeline.utfDecode.channel.mapError(cce => DecodeError.ReadError(Cause.fail(cce), cce.getMessage))
                ) >>> splitOnJsonBoundary >>>
                    ZPipeline.mapEitherChunked { (s: String) =>
                        jsonCodec.decodeJson(s).left.map(failure => DecodeError.ReadError(Cause.empty, failure))
                    }

            override def encode(value: A): Chunk[Byte] =
                JsonEncoder.charSequenceToByteChunk(jsonCodec.encodeJson(value, None))

            override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
                ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(streamEncoderSeparator).flattenChunks

    implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
        schemaBasedBinaryCodec[A](JsonCodec.Configuration.default)

    private object JsonSplitter:
        val validNumChars: Set[Char] = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'E', 'e', '-', '+', '.')
        val ContextJson              = 'j'
        val ContextString            = 's'
        val ContextBoolean           = 'b'
        val ContextNull              = 'u'
        val ContextNullAfterFirstL   = 'x'
        val ContextNumber            = 'n'
        val ContextEscape            = 'e'
        val ContextDone              = 'd'

        def jsonSplitter(wrappedInArray: Boolean): ZPipeline[Any, Nothing, String, String] =
            ZPipeline.suspend {
                val stringBuilder = new StringBuilder
                var depth         = if wrappedInArray then -1 else 0
                var context       = ContextJson

                def fetchChunk(chunk: Chunk[String]): Chunk[String] =
                    val chunkBuilder = ChunkBuilder.make[String]()
                    for
                        string <- chunk
                        c      <- string
                    do
                        var valueEnded = false
                        context match
                            case ContextEscape =>
                                context = 's'
                            case ContextString =>
                                c match
                                    case '\\' => context = ContextEscape
                                    case '"' =>
                                        context = ContextJson
                                        valueEnded = true
                                    case _ =>
                            case ContextBoolean =>
                                if c == 'e' then
                                    context = ContextJson
                                    valueEnded = true
                            case ContextNull =>
                                if c == 'l' then
                                    context = ContextNullAfterFirstL
                            case ContextNullAfterFirstL =>
                                if c == 'l' then
                                    context = ContextJson
                                    valueEnded = true
                            case ContextNumber =>
                                c match
                                    case '}' | ']' =>
                                        depth -= 1
                                        context = if depth < 0 then ContextDone else ContextJson
                                        valueEnded = true
                                    case _ if !validNumChars(c) =>
                                        context = ContextJson
                                        valueEnded = true
                                    case _ =>
                            case ContextDone => // no more values, ignore everything
                            case _ =>
                                c match
                                    case '{' | '[' =>
                                        depth += 1
                                    case '}' | ']' =>
                                        depth -= 1
                                        valueEnded = true
                                        if depth == -1 then context = ContextDone
                                    case '"' =>
                                        context = ContextString
                                    case 't' | 'f' =>
                                        context = ContextBoolean
                                    case 'n' =>
                                        context = ContextNull
                                    case x if validNumChars(x) =>
                                        context = ContextNumber
                                    case _ =>
                        end match
                        if context != ContextDone && (depth > 0 || context != ContextJson || valueEnded) then
                            stringBuilder.append(c)

                        if valueEnded && depth == 0 then
                            val str = stringBuilder.result()
                            if !str.forall(_.isWhitespace) then
                                chunkBuilder += str
                            stringBuilder.clear()
                        end if
                    end for
                    chunkBuilder.result()
                end fetchChunk

                lazy val loop: ZChannel[Any, ZNothing, Chunk[String], Any, Nothing, Chunk[String], Any] =
                    ZChannel.readWithCause(
                        in =>
                            val out = fetchChunk(in)
                            if out.isEmpty then loop else ZChannel.write(out) *> loop
                        ,
                        err =>
                            if stringBuilder.isEmpty then ZChannel.refailCause(err)
                            else ZChannel.write(Chunk.single(stringBuilder.result())) *> ZChannel.refailCause(err),
                        done =>
                            if stringBuilder.isEmpty then ZChannel.succeed(done)
                            else ZChannel.write(Chunk.single(stringBuilder.result())) *> ZChannel.succeed(done)
                    )

                ZPipeline.fromChannel(loop)
            }
    end JsonSplitter

    val splitOnJsonBoundary: ZPipeline[Any, Nothing, String, String]    = JsonSplitter.jsonSplitter(wrappedInArray = false)
    val splitJsonArrayElements: ZPipeline[Any, Nothing, String, String] = JsonSplitter.jsonSplitter(wrappedInArray = true)

    @deprecated("Use Configuration based method instead", "1.6.7")
    def schemaBasedBinaryCodec[A](cfg: Config)(implicit schema: Schema[A]): BinaryCodec[A] =
        schemaBasedBinaryCodec[A](cfg.toConfiguration)

    def schemaBasedBinaryCodec[A](cfg: Configuration)(implicit schema: Schema[A]): BinaryCodec[A] =
        new BinaryCodec[A]:
            override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
                JsonDecoder.decode(
                    schema,
                    new String(whole.toArray, JsonEncoder.CHARSET),
                    cfg
                )

            override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
                ZPipeline.utfDecode.mapError(cce => DecodeError.ReadError(Cause.fail(cce), cce.getMessage)) >>>
                    (if cfg.treatStreamsAsArrays then splitJsonArrayElements else splitOnJsonBoundary) >>>
                    ZPipeline.mapZIO { (s: String) =>
                        ZIO.fromEither(JsonDecoder.decode(schema, s, cfg))
                    }

            override def encode(value: A): Chunk[Byte] =
                JsonEncoder.encode(schema, value, cfg)

            override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
                if cfg.treatStreamsAsArrays then
                    val interspersed: ZPipeline[Any, Nothing, A, Byte] = ZPipeline
                        .mapChunks[A, Chunk[Byte]](_.map(encode))
                        .intersperse(Chunk.single(','.toByte))
                        .flattenChunks
                    val prepended: ZPipeline[Any, Nothing, A, Byte] =
                        interspersed >>> ZPipeline.prepend(Chunk.single('['.toByte))
                    prepended >>> ZPipeline.append(Chunk.single(']'.toByte))
                else
                    ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(Chunk.single('\n'.toByte)).flattenChunks

    def jsonEncoder[A](schema: Schema[A]): ZJsonEncoder[A] =
        JsonEncoder.schemaEncoder(schema, JsonCodec.Configuration.default)

    @deprecated("Use Configuration based method instead", "1.6.7")
    def jsonEncoder[A](cfg: JsonCodec.Config)(schema: Schema[A]): ZJsonEncoder[A] =
        JsonEncoder.schemaEncoder(schema, cfg.toConfiguration)

    def jsonEncoder[A](cfg: JsonCodec.Configuration)(schema: Schema[A]): ZJsonEncoder[A] =
        JsonEncoder.schemaEncoder(schema, cfg)

    def jsonDecoder[A](schema: Schema[A]): ZJsonDecoder[A] =
        JsonDecoder.schemaDecoder(schema, JsonCodec.Configuration.default)

    def jsonDecoder[A](cfg: JsonCodec.Configuration)(schema: Schema[A]): ZJsonDecoder[A] =
        JsonDecoder.schemaDecoder(schema, cfg)

    def jsonCodec[A](schema: Schema[A]): ZJsonCodec[A] =
        ZJsonCodec(jsonEncoder(schema), jsonDecoder(schema))

    @deprecated("Use Configuration based method instead", "1.6.7")
    def jsonCodec[A](cfg: JsonCodec.Config)(schema: Schema[A]): ZJsonCodec[A] =
        ZJsonCodec(jsonEncoder(cfg.toConfiguration)(schema), jsonDecoder(cfg.toConfiguration)(schema))

    def jsonCodec[A](cfg: JsonCodec.Configuration)(schema: Schema[A]): ZJsonCodec[A] =
        ZJsonCodec(jsonEncoder(cfg)(schema), jsonDecoder(cfg)(schema))

    object Codecs:
        protected[codec] val unitEncoder: ZJsonEncoder[Unit] =
            (_: Unit, _: Option[Int], out: Write) => out.write("{}")

        private[codec] val unitDecoder: ZJsonDecoder[Unit] =
            (trace: List[ZJsonDecoder.JsonError], in: RetractReader) =>
                Lexer.char(trace, in, '{')
                Lexer.char(trace, in, '}')
                ()

        protected[codec] val unitCodec: ZJsonCodec[Unit] = ZJsonCodec(unitEncoder, unitDecoder)

        protected[codec] def failDecoder[A](message: String): ZJsonDecoder[A] =
            (trace: List[ZJsonDecoder.JsonError], _: RetractReader) => Lexer.error(message, trace)

        private[codec] def primitiveCodec[A](standardType: StandardType[A]): ZJsonCodec[A] =
            standardType match
                case StandardType.UnitType           => unitCodec
                case StandardType.StringType         => ZJsonCodec.string
                case StandardType.BoolType           => ZJsonCodec.boolean
                case StandardType.ByteType           => ZJsonCodec.byte
                case StandardType.ShortType          => ZJsonCodec.short
                case StandardType.IntType            => ZJsonCodec.int
                case StandardType.LongType           => ZJsonCodec.long
                case StandardType.FloatType          => ZJsonCodec.float
                case StandardType.DoubleType         => ZJsonCodec.double
                case StandardType.BinaryType         => ZJsonCodec.chunk(using ZJsonCodec.byte.encoder, ZJsonCodec.byte.decoder)
                case StandardType.CharType           => ZJsonCodec.char
                case StandardType.BigIntegerType     => ZJsonCodec.bigInteger
                case StandardType.BigDecimalType     => ZJsonCodec.bigDecimal
                case StandardType.UUIDType           => ZJsonCodec.uuid
                case StandardType.DayOfWeekType      => ZJsonCodec.dayOfWeek      // ZJsonCodec[java.time.DayOfWeek]
                case StandardType.DurationType       => ZJsonCodec.duration       // ZJsonCodec[java.time.Duration]
                case StandardType.InstantType        => ZJsonCodec.instant        // ZJsonCodec[java.time.Instant]
                case StandardType.LocalDateType      => ZJsonCodec.localDate      // ZJsonCodec[java.time.LocalDate]
                case StandardType.LocalDateTimeType  => ZJsonCodec.localDateTime  // ZJsonCodec[java.time.LocalDateTime]
                case StandardType.LocalTimeType      => ZJsonCodec.localTime      // ZJsonCodec[java.time.LocalTime]
                case StandardType.MonthType          => ZJsonCodec.month          // ZJsonCodec[java.time.Month]
                case StandardType.MonthDayType       => ZJsonCodec.monthDay       // ZJsonCodec[java.time.MonthDay]
                case StandardType.OffsetDateTimeType => ZJsonCodec.offsetDateTime // ZJsonCodec[java.time.OffsetDateTime]
                case StandardType.OffsetTimeType     => ZJsonCodec.offsetTime     // ZJsonCodec[java.time.OffsetTime]
                case StandardType.PeriodType         => ZJsonCodec.period         // ZJsonCodec[java.time.Period]
                case StandardType.YearType           => ZJsonCodec.year           // ZJsonCodec[java.time.Year]
                case StandardType.YearMonthType      => ZJsonCodec.yearMonth      // ZJsonCodec[java.time.YearMonth]
                case StandardType.ZonedDateTimeType  => ZJsonCodec.zonedDateTime  // ZJsonCodec[java.time.ZonedDateTime]
                case StandardType.ZoneIdType         => ZJsonCodec.zoneId         // ZJsonCodec[java.time.ZoneId]
                case StandardType.ZoneOffsetType     => ZJsonCodec.zoneOffset     // ZJsonCodec[java.time.ZoneOffset]
                case StandardType.CurrencyType       => ZJsonCodec.currency       // ZJsonCodec[java.util.Currency]
    end Codecs

    object JsonEncoder:

        import Codecs.*
        import ProductEncoder.*
        import ZJsonEncoder.bump
        import ZJsonEncoder.pad

        private case class EncoderKey[A](schema: Schema[A], cfg: Configuration, discriminatorTuple: DiscriminatorTuple):
            override val hashCode: Int = System.identityHashCode(schema) ^ cfg.hashCode ^ discriminatorTuple.hashCode

            override def equals(obj: Any): Boolean = obj match
                case ek: EncoderKey[?] => (ek.schema eq schema) && ek.cfg == cfg && ek.discriminatorTuple == discriminatorTuple
                case _                 => false
        end EncoderKey

        private[codec] val CHARSET = StandardCharsets.UTF_8
        private[this] val encoders = new ConcurrentHashMap[EncoderKey[_], ZJsonEncoder[_]]()

        @deprecated(
            """Use JsonCodec.Configuration instead.
JsonCodec.Configuration makes it now possible to configure en-/decoding of empty collection and nulls (Options) independently.""",
            "1.6.7"
        )
        final def encode[A](schema: Schema[A], value: A, cfg: Config): Chunk[Byte] =
            encode(schema, value, cfg.toConfiguration)

        final def encode[A](schema: Schema[A], value: A, cfg: Configuration): Chunk[Byte] =
            charSequenceToByteChunk(schemaEncoder(schema, cfg).encodeJson(value, None))

        def format(caseName: String, cfg: Configuration): String =
            if cfg.discriminatorFormat == NameFormat.Identity then caseName
            else cfg.discriminatorFormat(caseName)

        private[codec] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] =
            val bytes = CHARSET.newEncoder().encode(CharBuffer.wrap(chars))
            Chunk.fromByteBuffer(bytes)

        private[codec] def schemaEncoder[A](
            schema: Schema[A],
            cfg: Configuration,
            discriminatorTuple: DiscriminatorTuple = None
        ): ZJsonEncoder[A] =
            val key                      = EncoderKey(schema, cfg, discriminatorTuple)
            var encoder: ZJsonEncoder[A] = encoders.get(key).asInstanceOf[ZJsonEncoder[A]]
            if encoder eq null then
                encoder = schemaEncoderSlow(schema, cfg, discriminatorTuple)
                encoders.put(key, encoder)
            encoder
        end schemaEncoder

        // scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
        private[this] def schemaEncoderSlow[A](schema: Schema[A], cfg: Configuration, discriminatorTuple: DiscriminatorTuple): ZJsonEncoder[A] =
            schema match
                case Schema.Primitive(standardType, _)           => primitiveCodec(standardType).encoder
                case Schema.Optional(schema, _)                  => ZJsonEncoder.option(schemaEncoder(schema, cfg))
                case Schema.Tuple2(l, r, _)                      => ZJsonEncoder.tuple2(schemaEncoder(l, cfg), schemaEncoder(r, cfg))
                case Schema.Sequence(schema, _, g, _, _)         => ZJsonEncoder.chunk(using schemaEncoder(schema, cfg)).contramap(g)
                case Schema.NonEmptySequence(schema, _, g, _, _) => ZJsonEncoder.chunk(using schemaEncoder(schema, cfg)).contramap(g)
                case Schema.Map(ks, vs, _)                       => mapEncoder(ks, vs, cfg)
                case Schema.NonEmptyMap(ks, vs, _)               => mapEncoder(ks, vs, cfg).contramap(_.toMap)
                case Schema.Set(s, _)                            => ZJsonEncoder.set(using schemaEncoder(s, cfg))
                case Schema.Transform(c, _, g, a, _)             => transformEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g, cfg, discriminatorTuple)
                case Schema.Fail(_, _)                           => unitEncoder.contramap(_ => ())
                case Schema.Either(left, right, _)               => ZJsonEncoder.either(using schemaEncoder(left, cfg), schemaEncoder(right, cfg))
                case Schema.Fallback(left, right, _, _)          => fallbackEncoder(schemaEncoder(left, cfg), schemaEncoder(right, cfg))
                case s: Schema.Lazy[A]                           => ZJsonEncoder.suspend(schemaEncoder(s.schema, cfg, discriminatorTuple))
                case s: Schema.GenericRecord                     => recordEncoder(s, cfg, discriminatorTuple)
                case s: Schema.Record[A]                         => caseClassEncoder(s, cfg, discriminatorTuple)
                case s: Schema.Enum[A]                           => enumEncoder(s, cfg)
                case s: Schema.Dynamic                           => dynamicEncoder(s, cfg)
                case null =>
                    throw new Exception(s"A captured schema is null, most likely due to wrong field initialization order")
        // scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

        private[codec] def transformFieldEncoder[A, B](
            schema: Schema[A],
            g: B => Either[String, A],
            cfg: Configuration
        ): Option[JsonFieldEncoder[B]] =
            jsonFieldEncoder(schema, cfg).map { fieldEncoder =>
                new JsonFieldEncoder[B]:
                    override def unsafeEncodeField(b: B): String =
                        g(b) match
                            case Right(a) => fieldEncoder.unsafeEncodeField(a)
                            case _        => throw new RuntimeException(s"Failed to encode field $b")
            }

        private[codec] def jsonFieldEncoder[A](schema: Schema[A], cfg: Configuration): Option[JsonFieldEncoder[A]] =
            schema match
                case Schema.Primitive(StandardType.StringType, _) => Option(JsonFieldEncoder.string)
                case Schema.Primitive(StandardType.LongType, _)   => Option(JsonFieldEncoder.long)
                case Schema.Primitive(StandardType.IntType, _)    => Option(JsonFieldEncoder.int)
                case Schema.Primitive(StandardType.UUIDType, _)   => Option(JsonFieldEncoder.uuid)
                case enum0: Schema.Enum[?] if enum0.annotations.exists(_.isInstanceOf[simpleEnum]) =>
                    Option(JsonFieldEncoder.string.contramap(caseMap(enum0, cfg)))
                case Schema.Transform(c, _, g, a, _) =>
                    transformFieldEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g, cfg)
                case Schema.Lazy(inner) => jsonFieldEncoder(inner(), cfg)
                case _                  => None

        private[codec] def mapEncoder[K, V](
            ks: Schema[K],
            vs: Schema[V],
            cfg: Configuration
        ): ZJsonEncoder[Map[K, V]] =
            val valueEncoder = JsonEncoder.schemaEncoder(vs, cfg)
            jsonFieldEncoder(ks, cfg) match
                case Some(jsonFieldEncoder) =>
                    ZJsonEncoder.map(using jsonFieldEncoder, valueEncoder)
                case None =>
                    ZJsonEncoder.chunk(using schemaEncoder(ks, cfg).zip(valueEncoder)).contramap(Chunk.fromIterable)
            end match
        end mapEncoder

        private[codec] def dynamicEncoder(
            schema: Schema.Dynamic,
            cfg: JsonCodec.Configuration
        ): ZJsonEncoder[DynamicValue] =
            if schema.annotations.exists(_.isInstanceOf[directDynamicMapping]) then
                new ZJsonEncoder[DynamicValue]:
                    directEncoder =>
                    override def unsafeEncode(value: DynamicValue, indent: Option[Int], out: Write): Unit =
                        value match
                            case DynamicValue.Record(_, values) =>
                                if values.isEmpty then
                                    out.write("{}")
                                else
                                    out.write('{')
                                    val indent_ = bump(indent)
                                    pad(indent_, out)
                                    var first = true
                                    values.foreach {
                                        case (key, value) =>
                                            if first then first = false
                                            else
                                                out.write(',')
                                                if indent.isDefined then pad(indent_, out)
                                            end if
                                            ZJsonEncoder.string.unsafeEncode(key, indent_, out)
                                            if indent.isEmpty then out.write(':')
                                            else out.write(" : ")
                                            directEncoder.unsafeEncode(value, indent_, out)
                                    }
                                    pad(indent, out)
                                    out.write('}')
                            case DynamicValue.Enumeration(_, _) =>
                                throw new Exception(
                                    s"DynamicValue.Enumeration is not supported in directDynamicMapping mode"
                                )
                            case DynamicValue.Sequence(values) =>
                                ZJsonEncoder.chunk(using directEncoder).unsafeEncode(values, indent, out)
                            case DynamicValue.Dictionary(_) =>
                                throw new Exception(
                                    s"DynamicValue.Dictionary is not supported in directDynamicMapping mode"
                                )
                            case DynamicValue.SetValue(values) =>
                                ZJsonEncoder.set(using directEncoder).unsafeEncode(values, indent, out)
                            case DynamicValue.Primitive(value, standardType) =>
                                primitiveCodec(standardType).encoder.unsafeEncode(value, indent, out)
                            case DynamicValue.Singleton(_) =>
                                out.write("{}")
                            case DynamicValue.SomeValue(value) =>
                                directEncoder.unsafeEncode(value, indent, out)
                            case DynamicValue.NoneValue =>
                                out.write("null")
                            case DynamicValue.Tuple(_, _) =>
                                throw new Exception(s"DynamicValue.Tuple is not supported in directDynamicMapping mode")
                            case DynamicValue.LeftValue(_) =>
                                throw new Exception(
                                    s"DynamicValue.LeftValue is not supported in directDynamicMapping mode"
                                )
                            case DynamicValue.RightValue(_) =>
                                throw new Exception(
                                    s"DynamicValue.RightValue is not supported in directDynamicMapping mode"
                                )
                            case DynamicValue.BothValue(left, right) =>
                                out.write('[')
                                val indent_ = bump(indent)
                                pad(indent_, out)
                                directEncoder.unsafeEncode(left, indent_, out)
                                out.write(',')
                                if indent.isDefined then ZJsonEncoder.pad(indent_, out)
                                directEncoder.unsafeEncode(right, indent_, out)
                                pad(indent, out)
                                out.write(']')
                            case DynamicValue.DynamicAst(_) =>
                                throw new Exception(
                                    s"DynamicValue.DynamicAst is not supported in directDynamicMapping mode"
                                )
                            case DynamicValue.Error(message) =>
                                throw new Exception(message)
            else
                schemaEncoder(DynamicValue.schema, cfg)

        private def transformEncoder[A, B](
            schema: Schema[A],
            g: B => Either[String, A],
            cfg: Configuration,
            discriminatorTuple: DiscriminatorTuple
        ): ZJsonEncoder[B] =
            new ZJsonEncoder[B]:
                private lazy val innerEncoder = schemaEncoder(schema, cfg, discriminatorTuple)

                override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
                    g(b) match
                        case Right(a) => innerEncoder.unsafeEncode(a, indent, out)
                        case _        => ()

                override def isNothing(b: B): Boolean =
                    g(b) match
                        case Right(a) => innerEncoder.isNothing(a)
                        case _        => false

        private def caseMap[Z](schema: Schema.Enum[Z], cfg: Configuration): Map[Z, String] =
            schema.nonTransientCases
                .map(case_ =>
                    Schema.force(case_.schema).asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct() ->
                        format(case_.caseName, cfg)
                )
                .toMap

        private def enumEncoder[Z](schema: Schema.Enum[Z], cfg: Configuration): ZJsonEncoder[Z] =
            // if all cases are CaseClass0, encode as a String
            if schema.annotations.exists(_.isInstanceOf[simpleEnum]) then
                val caseMap = JsonEncoder.caseMap(schema, cfg)
                ZJsonEncoder.string.contramap(caseMap(_))
            else
                val discriminatorName =
                    if schema.noDiscriminator || (cfg.noDiscriminator && schema.discriminatorName.isEmpty) then None
                    else schema.discriminatorName.orElse(cfg.discriminatorName)
                val doJsonObjectWrapping =
                    discriminatorName.isEmpty &&
                        !schema.noDiscriminator &&
                        !cfg.noDiscriminator
                if doJsonObjectWrapping then
                    new ZJsonEncoder[Z]:
                        private[this] val cases = schema.nonTransientCases.toArray
                        private[this] val decoders =
                            cases.map(case_ => schemaEncoder(case_.schema.asInstanceOf[Schema[Any]], cfg, None))
                        private[this] val encodedKeys =
                            cases.map(case_ =>
                                ZJsonEncoder.string.encodeJson(format(case_.caseName, cfg)).toString + ':'
                            )
                        private[this] val prettyEncodedKeys =
                            cases.map(case_ =>
                                ZJsonEncoder.string.encodeJson(format(case_.caseName, cfg)).toString + " : "
                            )

                        override def unsafeEncode(a: Z, indent: Option[Int], out: Write): Unit =
                            var idx = 0
                            while idx < cases.length do
                                val case_ = cases(idx)
                                if case_.isCase(a) then
                                    var indent_ = indent
                                    out.write('{')
                                    indent_ = bump(indent)
                                    pad(indent_, out)
                                    out.write(if indent_ eq None then encodedKeys(idx) else prettyEncodedKeys(idx))
                                    decoders(idx).unsafeEncode(case_.deconstruct(a), indent_, out)
                                    pad(indent, out)
                                    out.write('}')
                                    return
                                end if
                                idx += 1
                            end while
                            out.write("{}") // for transient cases
                        end unsafeEncode
                else
                    new ZJsonEncoder[Z]:
                        private[this] val cases = schema.nonTransientCases.toArray
                        private[this] val decoders = cases.map { case_ =>
                            val discriminatorTuple =
                                if discriminatorName eq None then None
                                else
                                    val key = ZJsonEncoder.string.encodeJson(discriminatorName.get, None).toString
                                    val value =
                                        ZJsonEncoder.string.encodeJson(format(case_.caseName, cfg), None).toString
                                    Some((key + ':' + value, key + " : " + value))
                            schemaEncoder(case_.schema.asInstanceOf[Schema[Any]], cfg, discriminatorTuple)
                        }

                        override def unsafeEncode(a: Z, indent: Option[Int], out: Write): Unit =
                            var idx = 0
                            while idx < cases.length do
                                val case_ = cases(idx)
                                if case_.isCase(a) then
                                    decoders(idx).unsafeEncode(case_.deconstruct(a), indent, out)
                                    return
                                idx += 1
                            end while
                            out.write("{}") // for transient cases
                        end unsafeEncode
                end if

        private def fallbackEncoder[A, B](left: ZJsonEncoder[A], right: ZJsonEncoder[B]): ZJsonEncoder[Fallback[A, B]] =
            new ZJsonEncoder[Fallback[A, B]]:

                def unsafeEncode(f: Fallback[A, B], indent: Option[Int], out: Write): Unit =
                    f match
                        case Fallback.Left(a)  => left.unsafeEncode(a, indent, out)
                        case Fallback.Right(b) => right.unsafeEncode(b, indent, out)
                        case Fallback.Both(a, b) =>
                            out.write('[')
                            val indent_ = bump(indent)
                            pad(indent_, out)
                            left.unsafeEncode(a, indent_, out)
                            out.write(',')
                            pad(indent_, out)
                            right.unsafeEncode(b, indent_, out)
                            pad(indent, out)
                            out.write(']')

        private def recordEncoder(
            schema: Schema.GenericRecord,
            cfg: Configuration,
            discriminatorTuple: DiscriminatorTuple
        ): ZJsonEncoder[ListMap[String, _]] =
            val nonTransientFields = schema.nonTransientFields.toArray
            if nonTransientFields.isEmpty then
                (_: ListMap[String, ?], _: Option[Int], out: Write) =>
                    out.write("{}")
            else
                val encoders =
                    nonTransientFields.map(field => schemaEncoder(field.schema.asInstanceOf[Schema[Any]], cfg))
                def name(field: Schema.Field[?, ?]): String =
                    if cfg.fieldNameFormat == NameFormat.Identity then field.fieldName
                    else if field.fieldName == field.name then cfg.fieldNameFormat(field.fieldName)
                    else field.fieldName
                val encodedNames =
                    nonTransientFields.map(field => ZJsonEncoder.string.encodeJson(name(field), None).toString + ':')
                val prettyEncodedNames =
                    nonTransientFields.map(field => ZJsonEncoder.string.encodeJson(name(field), None).toString + " : ")
                (a: ListMap[String, ?], indent: Option[Int], out: Write) =>
                    out.write('{')
                    val indent_ = bump(indent)
                    var comma   = false
                    if discriminatorTuple ne None then
                        comma = true
                        val tuple = discriminatorTuple.get
                        pad(indent_, out)
                        out.write(if indent_ eq None then tuple._1 else tuple._2)
                    end if
                    var idx = 0
                    while idx < nonTransientFields.length do
                        val field   = nonTransientFields(idx)
                        val encoder = encoders(idx)
                        val name    = field.fieldName
                        val value   = a(name)
                        if !isEmptyOptionalValue(field, value, cfg) && (!encoder.isNothing(value) || cfg.explicitNulls.encoding)
                        then
                            if comma then out.write(',')
                            else comma = true
                            pad(indent_, out)
                            out.write(if indent_ eq None then encodedNames(idx) else prettyEncodedNames(idx))
                            encoder.unsafeEncode(value, indent_, out)
                        end if
                        idx += 1
                    end while
                    pad(indent, out)
                    out.write('}')
            end if
        end recordEncoder
    end JsonEncoder

    object JsonDecoder:

        import Codecs.*
        import ProductDecoder.*

        private case class DecoderKey[A](schema: Schema[A], config: Configuration, discriminator: Option[String]):
            override val hashCode: Int = System.identityHashCode(schema) ^ config.hashCode ^ discriminator.hashCode

            override def equals(obj: Any): Boolean = obj match
                case dk: DecoderKey[?] =>
                    (dk.schema eq schema) && dk.config == config && dk.discriminator == discriminator
                case _ => false
        end DecoderKey

        private[this] val decoders = new ConcurrentHashMap[DecoderKey[_], ZJsonDecoder[_]]

        final def decode[A](schema: Schema[A], json: String, config: Configuration): Either[DecodeError, A] =
            schemaDecoder(schema, config).decodeJson(json) match
                case Left(value)  => Left(DecodeError.ReadError(Cause.empty, value))
                case Right(value) => Right(value)

        private[schema] def option[A](A: ZJsonDecoder[A]): ZJsonDecoder[Option[A]] =
            new ZJsonDecoder[Option[A]]:

                def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[A] =
                    (in.nextNonWhitespace(): @switch) match
                        case 'n' =>
                            val c1 = in.readChar()
                            val c2 = in.readChar()
                            val c3 = in.readChar()
                            if c1 != 'u' || c2 != 'l' || c3 != 'l' then Lexer.error("expected 'null'", trace)
                            None
                        case _ =>
                            in.retract()
                            Some(A.unsafeDecode(trace, in))

                override def unsafeDecodeMissing(trace: List[JsonError]): Option[A] = None

                final override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Option[A] =
                    json match
                        case Json.Null => None
                        case _         => Some(A.unsafeFromJsonAST(trace, json))

        private[codec] def schemaDecoder[A](
            schema: Schema[A],
            config: Configuration,
            discriminator: Option[String] = None
        ): ZJsonDecoder[A] =
            val key     = DecoderKey(schema, config, discriminator)
            var decoder = decoders.get(key).asInstanceOf[ZJsonDecoder[A]]
            if decoder eq null then
                decoder = schemaDecoderSlow(schema, discriminator, config)
                decoders.put(key, decoder)
            decoder
        end schemaDecoder

        // scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
        private[this] def schemaDecoderSlow[A](schema: Schema[A], discriminator: Option[String], config: Configuration): ZJsonDecoder[A] = schema match
            case Schema.Primitive(standardType, _)              => primitiveCodec(standardType).decoder
            case Schema.Optional(codec, _)                      => option(schemaDecoder(codec, config))
            case Schema.Tuple2(left, right, _)                  => ZJsonDecoder.tuple2(schemaDecoder(left, config), schemaDecoder(right, config))
            case Schema.Transform(c, f, _, a, _)                => schemaDecoder(a.foldLeft(c)((s, a) => s.annotate(a)), config, discriminator).mapOrFail(f)
            case Schema.Sequence(codec, f, _, _, _)             => ZJsonDecoder.chunk(using schemaDecoder(codec, config)).map(f)
            case s @ Schema.NonEmptySequence(codec, _, _, _, _) => ZJsonDecoder.chunk(using schemaDecoder(codec, config)).map(s.fromChunk)
            case Schema.Map(ks, vs, _)                          => mapDecoder(config)(ks, vs)
            case Schema.NonEmptyMap(ks, vs, _)                  => mapDecoder(config)(ks, vs).mapOrFail(m => NonEmptyMap.fromMapOption(m).toRight("NonEmptyMap expected"))
            case Schema.Set(s, _)                               => ZJsonDecoder.set(using schemaDecoder(s, config))
            case Schema.Fail(message, _)                        => failDecoder(message)
            case Schema.Either(left, right, _)                  => ZJsonDecoder.either(using schemaDecoder(left, config), schemaDecoder(right, config))
            case s @ Schema.Fallback(_, _, _, _)                => fallbackDecoder(s, config)
            case s: Schema.Lazy[A]                              => ZJsonDecoder.suspend(schemaDecoder(s.schema, config, discriminator))
            case s: Schema.GenericRecord                        => recordDecoder(s, discriminator, config)
            case s: Schema.Enum[A]                              => enumDecoder(s, config)
            // case Schema.Meta(_, _)                                                                           => astDecoder
            case s @ Schema.CaseClass0(_, _, _)                                => caseClass0Decoder(discriminator, s, config)
            case s @ Schema.CaseClass1(_, _, _, _)                             => caseClass1Decoder(discriminator, s, config)
            case s @ Schema.CaseClass2(_, _, _, _, _)                          => caseClass2Decoder(discriminator, s, config)
            case s @ Schema.CaseClass3(_, _, _, _, _, _)                       => caseClass3Decoder(discriminator, s, config)
            case s @ Schema.CaseClass4(_, _, _, _, _, _, _)                    => caseClass4Decoder(discriminator, s, config)
            case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _)                 => caseClass5Decoder(discriminator, s, config)
            case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _)              => caseClass6Decoder(discriminator, s, config)
            case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _)           => caseClass7Decoder(discriminator, s, config)
            case s @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _)        => caseClass8Decoder(discriminator, s, config)
            case s @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _)     => caseClass9Decoder(discriminator, s, config)
            case s @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _) => caseClass10Decoder(discriminator, s, config)
            case s @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass11Decoder(discriminator, s, config)
            case s @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass12Decoder(discriminator, s, config)
            case s @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass13Decoder(discriminator, s, config)
            case s @ Schema
                    .CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass14Decoder(discriminator, s, config)
            case s @ Schema
                    .CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass15Decoder(discriminator, s, config)
            case s @ Schema.CaseClass16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass16Decoder(discriminator, s, config)
            case s @ Schema.CaseClass17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass17Decoder(discriminator, s, config)
            case s @ Schema.CaseClass18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass18Decoder(discriminator, s, config)
            case s @ Schema.CaseClass19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass19Decoder(discriminator, s, config)
            case s @ Schema.CaseClass20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass20Decoder(discriminator, s, config)
            case s @ Schema.CaseClass21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass21Decoder(discriminator, s, config)
            case s @ Schema.CaseClass22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
                caseClass22Decoder(discriminator, s, config)
            case s: Schema.Dynamic => dynamicDecoder(s, config)
            case _                 => throw new Exception(s"Missing a handler for decoding of schema ${schema.toString}.")
        // scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

        private[codec] def mapDecoder[K, V](config: Configuration)(
            ks: Schema[K],
            vs: Schema[V]
        ): ZJsonDecoder[Map[K, V]] =
            val valueDecoder = JsonDecoder.schemaDecoder(vs, config)
            jsonFieldDecoder(ks, config) match
                case Some(jsonFieldDecoder) =>
                    ZJsonDecoder.map(using jsonFieldDecoder, valueDecoder)
                case None =>
                    ZJsonDecoder.chunk(using schemaDecoder(ks, config).zip(valueDecoder)).map(_.toMap)
            end match
        end mapDecoder

        private[codec] def jsonFieldDecoder[Z](schema: Schema[Z], cfg: Configuration): Option[JsonFieldDecoder[Z]] =
            schema match
                case Schema.Primitive(StandardType.StringType, _) => Option(JsonFieldDecoder.string)
                case Schema.Primitive(StandardType.LongType, _)   => Option(JsonFieldDecoder.long)
                case Schema.Primitive(StandardType.IntType, _)    => Option(JsonFieldDecoder.int)
                case Schema.Primitive(StandardType.UUIDType, _)   => Option(JsonFieldDecoder.uuid)
                case enum0: Schema.Enum[?] if enum0.annotations.exists(_.isInstanceOf[simpleEnum]) =>
                    Some {
                        val caseNameAliases =
                            JsonDecoder.caseNameAliases(enum0, cfg)
                        if caseNameAliases.size <= 64 then
                            new JsonFieldDecoder[Z]:
                                private[this] val stringMatrix = new StringMatrix(caseNameAliases.keys.toArray)
                                private[this] val cases = caseNameAliases.values.map { case_ =>
                                    case_.schema.asInstanceOf[Schema.CaseClass0[Any]].defaultConstruct()
                                }.toArray.asInstanceOf[Array[Z]]

                                override def unsafeDecodeField(trace: List[JsonError], in: String): Z =
                                    val idx =
                                        Lexer.enumeration(trace, new FastStringReader("\"" + in + "\""), stringMatrix)
                                    if idx < 0 then Lexer.error("unrecognized string", trace)
                                    cases(idx)
                                end unsafeDecodeField
                        else
                            new JsonFieldDecoder[Z]:
                                private[this] val cases = new util.HashMap[String, Z](caseNameAliases.size << 1)

                                caseNameAliases.foreach {
                                    case (name, case_) =>
                                        cases.put(
                                            name,
                                            case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct()
                                        )
                                }

                                override def unsafeDecodeField(trace: List[JsonError], in: String): Z =
                                    val result =
                                        cases.get(Lexer.string(trace, new FastStringReader("\"" + in + "\"")).toString)
                                    if result == null then Lexer.error("unrecognized string", trace)
                                    result
                                end unsafeDecodeField
                        end if

                    }
                case Schema.Transform(c, f, _, a, _) =>
                    jsonFieldDecoder(a.foldLeft(c)((s, a) => s.annotate(a)), cfg).map(_.mapOrFail(f))
                case Schema.Lazy(inner) => jsonFieldDecoder(inner(), cfg)
                case _                  => None

        private def dynamicDecoder(schema: Schema.Dynamic, config: Configuration): ZJsonDecoder[DynamicValue] =
            if schema.annotations.exists(_.isInstanceOf[directDynamicMapping]) then
                Json.decoder.map(jsonToDynamicValue)
            else
                schemaDecoder(DynamicValue.schema, config)

        private def jsonToDynamicValue(json: Json): DynamicValue =
            json match
                case Json.Obj(fields) =>
                    DynamicValue.Record(
                        TypeId.Structural,
                        ListMap(fields.map { case (k, v) => k -> jsonToDynamicValue(v) }*)
                    )
                case Json.Arr(elements) => DynamicValue.Sequence(elements.map(jsonToDynamicValue))
                case Json.Bool(value)   => DynamicValue.Primitive(value, StandardType.BoolType)
                case Json.Str(value)    => DynamicValue.Primitive(value, StandardType.StringType)
                case Json.Num(value)    => DynamicValue.Primitive(value, StandardType.BigDecimalType)
                case Json.Null          => DynamicValue.NoneValue

        private def enumDecoder[Z](parentSchema: Schema.Enum[Z], config: Configuration): ZJsonDecoder[Z] =

            val caseNameAliases: mutable.HashMap[String, Schema.Case[Z, Any]] =
                JsonDecoder.caseNameAliases(parentSchema, config)

            if parentSchema.cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])
            then // if all cases are CaseClass0, decode as String
                if caseNameAliases.size <= 64 then
                    new ZJsonDecoder[Z]:
                        private[this] val stringMatrix = new StringMatrix(caseNameAliases.keys.toArray)
                        private[this] val cases = caseNameAliases.values.map { case_ =>
                            case_.schema.asInstanceOf[Schema.CaseClass0[Any]].defaultConstruct()
                        }.toArray.asInstanceOf[Array[Z]]

                        override def unsafeDecode(trace: List[JsonError], in: RetractReader): Z =
                            val idx = Lexer.enumeration(trace, in, stringMatrix)
                            if idx < 0 then Lexer.error("unrecognized string", trace)
                            cases(idx)
                        end unsafeDecode
                else
                    new ZJsonDecoder[Z]:
                        private[this] val cases = new util.HashMap[String, Z](caseNameAliases.size << 1)

                        caseNameAliases.foreach {
                            case (name, case_) =>
                                cases.put(name, case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct())
                        }

                        override def unsafeDecode(trace: List[JsonError], in: RetractReader): Z =
                            val result = cases.get(Lexer.string(trace, in).toString)
                            if result == null then Lexer.error("unrecognized string", trace)
                            result
                        end unsafeDecode
            else if parentSchema.noDiscriminator || config.noDiscriminator then
                new ZJsonDecoder[Z]:
                    private[this] val decoders = parentSchema.cases.map(c => schemaDecoder(c.schema, config))

                    override def unsafeDecode(trace: List[JsonError], in: RetractReader): Z =
                        var rr = RecordingReader(in)
                        val it = decoders.iterator
                        while it.hasNext do
                            try
                                return it.next().unsafeDecode(trace, rr).asInstanceOf[Z]
                            catch
                                case ex if NonFatal(ex) =>
                                    rr.rewind()
                                    rr = RecordingReader(rr)
                        end while
                        Lexer.error("none of the subtypes could decode the data", trace)
                    end unsafeDecode
            else
                val discriminator = parentSchema.discriminatorName.orElse(config.discriminatorName)
                discriminator match
                    case None =>
                        if caseNameAliases.size <= 64 then
                            val caseMatrix = new StringMatrix(caseNameAliases.keys.toArray)
                            val cases = caseNameAliases.values.map { case_ =>
                                (
                                    JsonError.ObjectAccess(format(case_.caseName, config)),
                                    schemaDecoder(case_.schema, config)
                                )
                            }.toArray
                            (trace: List[JsonError], in: RetractReader) =>
                                val lexer = Lexer
                                lexer.char(trace, in, '{')
                                if !lexer.firstField(trace, in) then Lexer.error("missing subtype", trace)
                                val idx = lexer.field(trace, in, caseMatrix)
                                if idx < 0 then Lexer.error("unrecognized subtype", trace)
                                val spanWithDecoder = cases(idx)
                                val trace_          = spanWithDecoder._1 :: trace
                                val decoded         = spanWithDecoder._2.unsafeDecode(trace_, in).asInstanceOf[Z]
                                lexer.nextField(trace_, in)
                                decoded
                        else
                            val cases =
                                new util.HashMap[String, (JsonError.ObjectAccess, ZJsonDecoder[Any])](
                                    caseNameAliases.size << 1
                                )
                            caseNameAliases.foreach {
                                case (name, case_) =>
                                    cases.put(
                                        name,
                                        (
                                            JsonError.ObjectAccess(format(case_.caseName, config)),
                                            schemaDecoder(case_.schema, config)
                                        )
                                    )
                            }
                            (trace: List[JsonError], in: RetractReader) =>
                                val lexer = Lexer
                                lexer.char(trace, in, '{')
                                if !lexer.firstField(trace, in) then lexer.error("missing subtype", trace)
                                val fieldName       = lexer.string(trace, in).toString
                                val spanWithDecoder = cases.get(fieldName)
                                if spanWithDecoder eq null then lexer.error("unrecognized subtype", trace)
                                val trace_ = spanWithDecoder._1 :: trace
                                lexer.char(trace_, in, ':')
                                val decoded = spanWithDecoder._2.unsafeDecode(trace_, in).asInstanceOf[Z]
                                lexer.nextField(trace_, in)
                                decoded
                    case Some(discrName) =>
                        val discriminatorMatrix = new StringMatrix(Array(discrName))
                        val discriminatorSpan   = JsonError.ObjectAccess(discrName)
                        if caseNameAliases.size <= 64 then
                            val caseMatrix = new StringMatrix(caseNameAliases.keys.toArray)
                            val cases = caseNameAliases.values.map { case_ =>
                                (
                                    JsonError.ObjectAccess(format(case_.caseName, config)),
                                    schemaDecoder(case_.schema, config, discriminator)
                                )
                            }.toArray
                            (trace: List[JsonError], in: RetractReader) =>
                                val lexer = Lexer
                                lexer.char(trace, in, '{')
                                if !lexer.firstField(trace, in) then lexer.error("missing subtype", trace)
                                val rr = RecordingReader(in)
                                while {
                                    (lexer.field(trace, rr, discriminatorMatrix) < 0) && {
                                        lexer.skipValue(trace, rr)
                                        lexer.nextField(trace, rr) || lexer.error("missing subtype", trace)
                                    }
                                } do ()
                                end while
                                val trace_ = discriminatorSpan :: trace
                                val idx    = lexer.enumeration(trace_, rr, caseMatrix)
                                rr.rewind()
                                if idx < 0 then lexer.error("unrecognized subtype", trace_)
                                val spanWithDecoder = cases(idx)
                                spanWithDecoder._2.unsafeDecode(spanWithDecoder._1 :: trace_, rr).asInstanceOf[Z]
                        else
                            val cases =
                                new util.HashMap[String, (JsonError.ObjectAccess, ZJsonDecoder[Any])](
                                    caseNameAliases.size << 1
                                )
                            caseNameAliases.foreach {
                                case (name, case_) =>
                                    cases.put(
                                        name,
                                        (
                                            JsonError.ObjectAccess(format(case_.caseName, config)),
                                            schemaDecoder(case_.schema, config, discriminator)
                                        )
                                    )
                            }
                            (trace: List[JsonError], in: RetractReader) =>
                                val lexer = Lexer
                                lexer.char(trace, in, '{')
                                if !lexer.firstField(trace, in) then lexer.error("missing subtype", trace)
                                val rr = RecordingReader(in)
                                while {
                                    (lexer.field(trace, rr, discriminatorMatrix) < 0) && {
                                        lexer.skipValue(trace, rr)
                                        lexer.nextField(trace, rr) || lexer.error("missing subtype", trace)
                                    }
                                } do ()
                                end while
                                val trace_     = discriminatorSpan :: trace
                                val fieldValue = lexer.string(trace_, rr).toString
                                rr.rewind()
                                val spanWithDecoder = cases.get(fieldValue)
                                if spanWithDecoder eq null then lexer.error("unrecognized subtype", trace_)
                                spanWithDecoder._2.unsafeDecode(spanWithDecoder._1 :: trace_, rr).asInstanceOf[Z]
                        end if
                end match
            end if
        end enumDecoder

        private def format(caseName: String, cfg: Configuration): String =
            if cfg.discriminatorFormat == NameFormat.Identity then caseName
            else cfg.discriminatorFormat(caseName)

        private def caseNameAliases[Z](parentSchema: Schema.Enum[Z], config: Configuration) =
            val caseNameAliases = new mutable.HashMap[String, Schema.Case[Z, Any]]
            parentSchema.cases.foreach { case_ =>
                val schema = case_.asInstanceOf[Schema.Case[Z, Any]]
                caseNameAliases.put(format(case_.caseName, config), schema)
                case_.caseNameAliases.foreach(a => caseNameAliases.put(a, schema))
            }
            caseNameAliases
        end caseNameAliases

        private def recordDecoder(
            schema: GenericRecord,
            discriminator: Option[String],
            config: Configuration
        ): ZJsonDecoder[ListMap[String, Any]] =
            val explicitEmptyCollections = config.explicitEmptyCollections.decoding
            val explicitNulls            = config.explicitNulls.decoding
            if schema.fields.foldLeft(0)(_ + _.nameAndAliases.size) <= 64 then
                val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
                (trace: List[JsonError], in: RetractReader) => ccjd.unsafeDecodeListMap(trace, in)
            else
                new ZJsonDecoder[ListMap[String, Any]]:
                    private[this] val fields = schema.fields.toArray
                    private[this] val spansWithDecoders =
                        new util.HashMap[String, (JsonError.ObjectAccess, ZJsonDecoder[Any])](fields.length << 1):
                            fields.foreach { field =>
                                val spanWithDecoder =
                                    (
                                        JsonError.ObjectAccess(field.fieldName),
                                        schemaDecoder(field.schema, config).asInstanceOf[ZJsonDecoder[Any]]
                                    )
                                field.nameAndAliases.foreach(put(_, spanWithDecoder))
                            }
                    private[this] val skipExtraFields = !schema.annotations.exists(_.isInstanceOf[rejectExtraFields])

                    override def unsafeDecode(trace: List[JsonError], in: RetractReader): ListMap[String, Any] =
                        val lexer    = Lexer
                        var continue = true
                        if discriminator eq None then
                            lexer.char(trace, in, '{')
                            continue = lexer.firstField(trace, in)
                        val map = new util.HashMap[String, Any](fields.length << 1)
                        while continue do
                            val fieldNameOrAlias = lexer.string(trace, in).toString
                            val spanWithDecoder  = spansWithDecoders.get(fieldNameOrAlias)
                            if spanWithDecoder ne null then
                                val span   = spanWithDecoder._1
                                val dec    = spanWithDecoder._2
                                val trace_ = span :: trace
                                lexer.char(trace_, in, ':')
                                val fieldName = span.field // reuse strings with calculated hashCode
                                val prev      = map.put(fieldName, dec.unsafeDecode(trace_, in))
                                if prev != null then lexer.error("duplicate", trace_)
                            else if skipExtraFields || discriminator.contains(fieldNameOrAlias) then
                                lexer.char(trace, in, ':')
                                lexer.skipValue(trace, in)
                            else lexer.error("extra field", trace)
                            end if
                            continue = lexer.nextField(trace, in)
                        end while
                        var idx = 0
                        while idx < fields.length do
                            val field = fields(idx)
                            idx += 1
                            val fieldName = field.fieldName // reuse strings with calculated hashCode
                            if map.get(fieldName) == null then
                                map.put( // mitigation of a linking error for `map.computeIfAbsent` in Scala.js
                                    fieldName, {
                                        if (field.optional || field.transient) && field.defaultValue.isDefined then
                                            field.defaultValue.get
                                        else
                                            var schema = field.schema
                                            schema match
                                                case l: Schema.Lazy[?] => schema = l.schema
                                                case _                 =>
                                            schema match
                                                case _: Schema.Optional[?] if !explicitNulls => None
                                                case collection: Schema.Collection[?, ?] if !explicitEmptyCollections =>
                                                    collection.empty
                                                case _ =>
                                                    lexer.error("missing", spansWithDecoders.get(fieldName)._1 :: trace)
                                            end match
                                    }
                                )
                            end if
                        end while
                        (ListMap.newBuilder[String, Any] ++= ({ // to avoid O(n) insert operations
                            import scala.collection.JavaConverters.mapAsScalaMapConverter // use deprecated class for Scala 2.12 compatibility

                            map.asScala
                        }: @scala.annotation.nowarn)).result()
                    end unsafeDecode
            end if
        end recordDecoder

        private def fallbackDecoder[A, B](
            schema: Schema.Fallback[A, B],
            config: Configuration
        ): ZJsonDecoder[Fallback[A, B]] =
            new ZJsonDecoder[Fallback[A, B]]:
                private[this] val leftDecoder  = schemaDecoder(schema.left, config)
                private[this] val rightDecoder = schemaDecoder(schema.right, config)

                def unsafeDecode(trace: List[JsonError], in: RetractReader): Fallback[A, B] =
                    var left: Option[A]  = None
                    var right: Option[B] = None
                    try
                        // If this doesn't throw exception, it is an array, so it encodes a `Fallback.Both`
                        val lexer = Lexer
                        lexer.char(trace, in, '[')

                        // get left element
                        if lexer.firstArrayElement(in) then
                            val trace_ = JsonError.ArrayAccess(0) :: trace
                            try left = Some(leftDecoder.unsafeDecode(trace_, in))
                            catch
                                case _: UnsafeJson => ()

                            // read until ',' if left wasn't decoded
                            var continue = true
                            while continue do
                                try
                                    lexer.nextArrayElement(trace, in)
                                    continue = false
                                catch
                                    case _: UnsafeJson => ()
                            end while
                        end if

                        // get right element
                        if ((left eq None) || schema.fullDecode) && lexer.firstArrayElement(in) then
                            val trace_ = JsonError.ArrayAccess(1) :: trace
                            try right = Some(rightDecoder.unsafeDecode(trace_, in))
                            catch
                                case _: UnsafeJson => ()
                            try lexer.nextArrayElement(trace, in)
                            catch
                                case _: UnsafeJson => ()
                        end if

                    catch
                        // It's not an array, so it is of type A or B
                        case _: UnsafeJson =>
                            in.retract()
                            val rr = RecordingReader(in)
                            try
                                left = Some(leftDecoder.unsafeDecode(trace, rr))
                            catch
                                case UnsafeJson(_) =>
                                    rr.rewind()
                                    right = Some(rightDecoder.unsafeDecode(trace, rr))
                            end try
                    end try

                    (left, right) match
                        case (Some(a), Some(b)) => Fallback.Both(a, b)
                        case (Some(a), _)       => Fallback.Left(a)
                        case (_, Some(b))       => Fallback.Right(b)
                        case _ => Lexer.error("Fallback decoder was unable to decode both left and right sides", trace)
                    end match
                end unsafeDecode
    end JsonDecoder

    // scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] object ProductEncoder:
        import ZJsonEncoder.bump
        import ZJsonEncoder.pad

        private[codec] def isEmptyOptionalValue(schema: Schema.Field[?, ?], value: Any, cfg: Configuration) =
            (!cfg.explicitEmptyCollections.encoding || schema.optional) && (value match
                case None            => true
                case it: Iterable[?] => it.isEmpty
                case _               => false
            )

        private[codec] def caseClassEncoder[Z](schema: Schema.Record[Z], cfg: Configuration, discriminatorTuple: DiscriminatorTuple): ZJsonEncoder[Z] =
            val nonTransientFields = schema.nonTransientFields.toArray.asInstanceOf[Array[Schema.Field[Z, Any]]]
            val encoders           = nonTransientFields.map(s => JsonEncoder.schemaEncoder(s.schema, cfg))
            def name(field: Schema.Field[?, ?]): String =
                if cfg.fieldNameFormat == NameFormat.Identity then field.fieldName
                else if field.fieldName == field.name then cfg.fieldNameFormat(field.fieldName)
                else field.fieldName
            val encodedNames =
                nonTransientFields.map(field => ZJsonEncoder.string.encodeJson(name(field), None).toString + ':')
            val prettyEncodedNames =
                nonTransientFields.map(field => ZJsonEncoder.string.encodeJson(name(field), None).toString + " : ")
            (a: Z, indent: Option[Int], out: Write) =>
                out.write('{')
                val indent_ = bump(indent)
                var comma   = false
                if discriminatorTuple ne None then
                    comma = true
                    pad(indent_, out)
                    val tuple = discriminatorTuple.get
                    out.write(if indent_ eq None then tuple._1 else tuple._2)
                end if
                var idx = 0
                while idx < nonTransientFields.length do
                    val field   = nonTransientFields(idx)
                    val encoder = encoders(idx)
                    val value   = field.get(a)
                    if !isEmptyOptionalValue(field, value, cfg) && (!encoder.isNothing(value) || cfg.explicitNulls.encoding) then
                        if comma then out.write(',')
                        else comma = true
                        pad(indent_, out)
                        out.write(if indent_ eq None then encodedNames(idx) else prettyEncodedNames(idx))
                        encoder.unsafeEncode(value, indent_, out)
                    end if
                    idx += 1
                end while
                pad(indent, out)
                out.write('}')
        end caseClassEncoder
    end ProductEncoder

    // scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] object ProductDecoder:

        private[codec] def caseClass0Decoder[Z](discriminator: Option[String], schema: Schema.CaseClass0[Z], config: Configuration): ZJsonDecoder[Z] =
            val rejectExtraFields = schema.rejectExtraFields || config.rejectExtraFields
            val noDiscriminator   = discriminator.isEmpty
            (trace: List[JsonError], in: RetractReader) =>
                val lexer = Lexer
                if noDiscriminator then lexer.char(trace, in, '{')
                var continue = lexer.firstField(trace, in)
                while continue do
                    if rejectExtraFields then lexer.error("extra field", trace)
                    lexer.char(trace, in, '"')
                    lexer.skipString(trace, in)
                    lexer.char(trace, in, ':')
                    lexer.skipValue(trace, in)
                    continue = lexer.nextField(trace, in)
                end while
                schema.defaultConstruct()
        end caseClass0Decoder

        private[codec] def caseClass1Decoder[A, Z](discriminator: Option[String], schema: Schema.CaseClass1[A, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.defaultConstruct(buffer(0).asInstanceOf[A])
        end caseClass1Decoder

        private[codec] def caseClass2Decoder[A1, A2, Z](discriminator: Option[String], schema: Schema.CaseClass2[A1, A2, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])
        end caseClass2Decoder

        private[codec] def caseClass3Decoder[A1, A2, A3, Z](discriminator: Option[String], schema: Schema.CaseClass3[A1, A2, A3, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])
        end caseClass3Decoder

        private[codec] def caseClass4Decoder[A1, A2, A3, A4, Z](discriminator: Option[String], schema: Schema.CaseClass4[A1, A2, A3, A4, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])
        end caseClass4Decoder

        private[codec] def caseClass5Decoder[A1, A2, A3, A4, A5, Z](discriminator: Option[String], schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])
        end caseClass5Decoder

        private[codec] def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](discriminator: Option[String], schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])
        end caseClass6Decoder

        private[codec] def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](discriminator: Option[String], schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])
        end caseClass7Decoder

        private[codec] def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](discriminator: Option[String], schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])
        end caseClass8Decoder

        private[codec] def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](discriminator: Option[String], schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])
        end caseClass9Decoder

        private[codec] def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](discriminator: Option[String], schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])
        end caseClass10Decoder

        private[codec] def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](discriminator: Option[String], schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])
        end caseClass11Decoder

        private[codec] def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](discriminator: Option[String], schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])
        end caseClass12Decoder

        private[codec] def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](discriminator: Option[String], schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13]
                )
        end caseClass13Decoder

        private[codec] def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](discriminator: Option[String], schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14]
                )
        end caseClass14Decoder

        private[codec] def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](discriminator: Option[String], schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15]
                )
        end caseClass15Decoder

        private[codec] def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](discriminator: Option[String], schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16]
                )
        end caseClass16Decoder

        private[codec] def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](discriminator: Option[String], schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16],
                    buffer(16).asInstanceOf[A17]
                )
        end caseClass17Decoder

        private[codec] def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](discriminator: Option[String], schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16],
                    buffer(16).asInstanceOf[A17],
                    buffer(17).asInstanceOf[A18]
                )
        end caseClass18Decoder

        private[codec] def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](discriminator: Option[String], schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16],
                    buffer(16).asInstanceOf[A17],
                    buffer(17).asInstanceOf[A18],
                    buffer(18).asInstanceOf[A19]
                )
        end caseClass19Decoder

        private[codec] def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](discriminator: Option[String], schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16],
                    buffer(16).asInstanceOf[A17],
                    buffer(17).asInstanceOf[A18],
                    buffer(18).asInstanceOf[A19],
                    buffer(19).asInstanceOf[A20]
                )
        end caseClass20Decoder

        private[codec] def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](discriminator: Option[String], schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16],
                    buffer(16).asInstanceOf[A17],
                    buffer(17).asInstanceOf[A18],
                    buffer(18).asInstanceOf[A19],
                    buffer(19).asInstanceOf[A20],
                    buffer(20).asInstanceOf[A21]
                )
        end caseClass21Decoder

        private[codec] def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](discriminator: Option[String], schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z], config: Configuration): ZJsonDecoder[Z] =
            val ccjd = CaseClassJsonDecoder(schema, discriminator, config)
            (trace: List[JsonError], in: RetractReader) =>
                val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
                schema.construct(
                    buffer(0).asInstanceOf[A1],
                    buffer(1).asInstanceOf[A2],
                    buffer(2).asInstanceOf[A3],
                    buffer(3).asInstanceOf[A4],
                    buffer(4).asInstanceOf[A5],
                    buffer(5).asInstanceOf[A6],
                    buffer(6).asInstanceOf[A7],
                    buffer(7).asInstanceOf[A8],
                    buffer(8).asInstanceOf[A9],
                    buffer(9).asInstanceOf[A10],
                    buffer(10).asInstanceOf[A11],
                    buffer(11).asInstanceOf[A12],
                    buffer(12).asInstanceOf[A13],
                    buffer(13).asInstanceOf[A14],
                    buffer(14).asInstanceOf[A15],
                    buffer(15).asInstanceOf[A16],
                    buffer(16).asInstanceOf[A17],
                    buffer(17).asInstanceOf[A18],
                    buffer(18).asInstanceOf[A19],
                    buffer(19).asInstanceOf[A20],
                    buffer(20).asInstanceOf[A21],
                    buffer(21).asInstanceOf[A22]
                )
        end caseClass22Decoder
    end ProductDecoder
    // scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private class CaseClassJsonDecoder[Z](
        fields: Array[Schema.Field[Z, _]],
        fieldDecoders: Array[ZJsonDecoder[_]],
        spans: Array[JsonError.ObjectAccess],
        stringMatrix: StringMatrix,
        noDiscriminator: Boolean,
        skipExtraFields: Boolean,
        config: Configuration
    ):

        private val explicitEmptyCollections = config.explicitEmptyCollections.decoding
        private val explicitNulls            = config.explicitNulls.decoding

        def unsafeDecodeListMap(trace: List[JsonError], in: RetractReader): ListMap[String, Any] =
            val buffer = unsafeDecodeFields(trace, in)
            (ListMap.newBuilder[String, Any] ++= new collection.Map[
                String,
                Any
            ]: // only `.iterator` method will be called
                override def +[Any](kv: (String, Any)): collection.Map[String, Any] =
                    throw new UnsupportedOperationException("Not needed for internal use")

                override def -(key: String): collection.Map[String, Any] =
                    throw new UnsupportedOperationException("Not needed for internal use")

                override def -(key1: String, key2: String, keys: String*): collection.Map[String, Any] =
                    throw new UnsupportedOperationException("Not needed for internal use")

                override def get(key: String): Option[Any] =
                    throw new UnsupportedOperationException("Not needed for internal use")

                override def iterator: Iterator[(String, Any)] = new Iterator[(String, Any)]:
                    private[this] val keys    = spans
                    private[this] val values  = buffer
                    private[this] var nextIdx = 0

                    def hasNext: Boolean = nextIdx < values.length

                    @inline
                    def next(): (String, Any) =
                        val idx = nextIdx
                        nextIdx += 1
                        (keys(idx).field, values(idx))
                    end next
            ).result()
        end unsafeDecodeListMap

        def unsafeDecodeFields(trace: List[JsonError], in: RetractReader): Array[Any] =
            val lexer    = Lexer
            var continue = true
            if noDiscriminator then
                lexer.char(trace, in, '{')
                continue = lexer.firstField(trace, in)
            val len    = fields.length
            val buffer = new Array[Any](len)
            while continue do
                val idx = lexer.field(trace, in, stringMatrix)
                if idx >= 0 then
                    val trace_ = spans(idx) :: trace
                    if idx == len then lexer.skipValue(trace_, in) // skipping discriminator field values
                    else if buffer(idx) == null then buffer(idx) = fieldDecoders(idx).unsafeDecode(trace_, in)
                    else lexer.error("duplicate", trace_)
                else if skipExtraFields then lexer.skipValue(trace, in)
                else lexer.error("extra field", trace)
                end if
                continue = lexer.nextField(trace, in)
            end while
            var idx = 0
            while idx < len do
                if buffer(idx) == null then
                    val field = fields(idx)
                    if (field.optional || field.transient) && field.defaultValue.isDefined then
                        buffer(idx) = field.defaultValue.get
                    else
                        var schema = field.schema
                        schema match
                            case l: Schema.Lazy[?] => schema = l.schema
                            case _                 =>
                        buffer(idx) = schema match
                            case _: Schema.Optional[?] if !explicitNulls                          => None
                            case collection: Schema.Collection[?, ?] if !explicitEmptyCollections => collection.empty
                            case _ => lexer.error("missing", spans(idx) :: trace)
                    end if
                end if
                idx += 1
            end while
            buffer
        end unsafeDecodeFields
    end CaseClassJsonDecoder

    private object CaseClassJsonDecoder:

        def apply[Z](
            schema: Schema.Record[Z],
            discriminator: Option[String],
            config: Configuration
        ): CaseClassJsonDecoder[Z] =
            val hasDiscriminator = discriminator.isDefined
            val len              = schema.fields.length
            var nameLen          = len
            if hasDiscriminator then nameLen += 1
            val aliasLen = schema.fields.foldLeft(0)(_ + _.nameAndAliases.size) - len
            val fields   = new Array[Schema.Field[Z, _]](len)
            val decoders = new Array[ZJsonDecoder[_]](len)
            val spans    = new Array[JsonError.ObjectAccess](nameLen)
            val names    = new Array[String](nameLen)
            val aliases  = new Array[(String, Int)](aliasLen)
            var idx      = 0
            var aliasIdx = 0
            schema.fields.foreach { field =>
                fields(idx) = field
                decoders(idx) = schemaDecoder(field.schema, config)
                val name =
                    if config.fieldNameFormat == NameFormat.Identity then field.fieldName
                    else if field.fieldName == field.name then config.fieldNameFormat(field.fieldName)
                    else field.fieldName
                names(idx) = name
                spans(idx) = JsonError.ObjectAccess(name)
                (field.nameAndAliases - field.fieldName).foreach { a =>
                    aliases(aliasIdx) = (a, idx)
                    aliasIdx += 1
                }
                idx += 1
            }
            if hasDiscriminator then
                val discriminatorName = discriminator.get
                names(idx) = discriminatorName
                spans(idx) = JsonError.ObjectAccess(discriminatorName)
            end if
            new CaseClassJsonDecoder(
                fields,
                decoders,
                spans,
                new StringMatrix(names, aliases),
                !hasDiscriminator,
                !schema.rejectExtraFields && !config.rejectExtraFields,
                config
            )
        end apply
    end CaseClassJsonDecoder
end JsonCodec
