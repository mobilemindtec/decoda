package io.json.codec.converter

import base.*
import io.json.codec.JsonCodecException
import io.json.codec.infra.macros.createInstance

import scala.collection.mutable

case class DecodeOptions(pattern: String = "", df: Option[DateFormatter] = None)

type DecoderFactory[T] = () => T

trait DataDecoder[T]:
  def decode(v: Any): T

case class DecodeMetaData[T, S](
    field: String,
    decoder: DataDecoder[S],
    opts: Option[DecodeOptions],
    f: (T, S) => T
):

  def fnApply(obj: T, v: Any): T =
    f(obj, decoder.decode(v))

class Decoder[T]()(using
    factory: DecoderFactory[T],
    parser: JsonParser
) extends DataDecoder[T]:

  private val fns = mutable.Buffer.empty[DecodeMetaData[T, ?]]

  override def decode(v: Any): T =
    v match
      case s: String => parse(s)
      case jsonObject: JsonObject =>
        fns.foldLeft(instance) { (inst, decFn) =>
          jsonObject.getByName(decFn.field) match
            case Some(v) =>
              decFn.fnApply(inst, v)
            case _ => inst
        }
      case _ => throw new JsonCodecException(s"can't decode $v")

  def add(fn: DecodeMetaData[T, ?]): Decoder[T] =
    fns.addOne(fn)
    this

  private def instance: T = factory()

  def parse(s: String): T =
    decode(parser.parse(s))

object Decoder:

  type DecoderField[T, S] = DataDecoder[S] ?=> Decoder[T]
  type DecoderCreator[T] = JsonParser ?=> Decoder[T]

  given DataDecoder[String] with
    override def decode(v: Any): String =
      v match
        case s: String => s
        case null      => null
        case _         => throw new JsonCodecException(s"can't parse $v to String")

  given DataDecoder[Short] with
    override def decode(v: Any): Short =
      v match
        case i: Int   => i.toShort
        case i: Long  => i.toShort
        case i: Short => i
        case _        => throw new JsonCodecException(s"can't parse $v to Short")

  given DataDecoder[Int] with
    override def decode(v: Any): Int =
      v match
        case i: Int   => i
        case i: Long  => i.toInt
        case i: Short => i.toInt
        case _        => throw new JsonCodecException(s"can't parse $v to Int")

  given DataDecoder[Long] with
    override def decode(v: Any): Long =
      v match
        case i: Int   => i.toLong
        case i: Long  => i
        case i: Short => i.toLong
        case _        => throw new JsonCodecException(s"can't parse $v to Long")

  given DataDecoder[Float] with
    override def decode(v: Any): Float =
      v match
        case i: Float  => i
        case i: Double => i.toFloat
        case _         => throw new JsonCodecException(s"can't parse $v to Float")

  given DataDecoder[Double] with
    override def decode(v: Any): Double =
      v match
        case i: Float  => i.toDouble
        case i: Double => i
        case _         => throw new JsonCodecException(s"can't parse $v to Double")

  given DataDecoder[Boolean] with
    override def decode(v: Any): Boolean =
      v match
        case i: Boolean => i
        case i: String  => i.toLowerCase == "true"
        case _          => throw new JsonCodecException(s"can't parse $v to Boolean")

  given OptionDecoderCodec[T](using decoder: DataDecoder[T]): DataDecoder[Option[T]] with
    override def decode(v: Any): Option[T] =
      if v == null
      then None
      else Some(decoder.decode(v))

  given SeqDecoderCodec[T](using decoder: DataDecoder[T]): DataDecoder[Seq[T]] with
    override def decode(v: Any): Seq[T] =
      v match
        case arr: JsonArray =>
          arr.map(decoder.decode)
        case _ => Seq.empty

  given ListDecoderCodec[T](using decoder: DataDecoder[T]): DataDecoder[List[T]] with
    override def decode(v: Any): List[T] =
      v match
        case arr: JsonArray =>
          arr.map(decoder.decode)
        case _ => List.empty

  given SetDecoderCodec[T](using decoder: DataDecoder[T]): DataDecoder[Set[T]] with
    override def decode(v: Any): Set[T] =
      v match
        case arr: JsonArray =>
          arr.map(decoder.decode).toSet
        case _ => Set.empty

  inline def typ[T]: DecoderCreator[T] =
    given creator: (() => T) = () => createInstance[T]
    new Decoder

  inline def field[T, A](name: String, f: (T, A) => T)(
      decoder: Decoder[T]
  ): DecoderField[T, A] =
    decoder.add(DecodeMetaData[T, A](name, summon[DataDecoder[A]], None, f))

  inline def field[T, A](name: String, opts: DecodeOptions, f: (T, A) => T)(
      decoder: Decoder[T]
  ): DecoderField[T, A] =
    decoder.add(DecodeMetaData[T, A](name, summon[DataDecoder[A]], Some(opts), f))
