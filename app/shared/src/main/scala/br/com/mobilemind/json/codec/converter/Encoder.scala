package br.com.mobilemind.json.codec.converter

import br.com.mobilemind.json.codec.JsonCodecException
import br.com.mobilemind.json.codec.converter.OptOmit.NoOmit
import br.com.mobilemind.json.codec.converter.base.*

import java.util.Date
import scala.collection.mutable

case class EncodeOptions(
    opt: OptOmit = NoOmit,
    pattern: String = "",
    df: Option[DateFormatter] = None
)

trait DataEncoder[T]:
  def encode(v: T): Any

case class EncoderMedaTada[T, S](
    name: String,
    opts: Option[EncodeOptions] = None,
    encoder: DataEncoder[S],
    f: T => S
):
  def fnApply(obj: T): Any =
    encoder.encode(f(obj))

class Encoder[T](using jsonCreator: JsonCreator) extends DataEncoder[T]:
  private val fns = mutable.Buffer.empty[EncoderMedaTada[T, ?]]

  def add(fn: EncoderMedaTada[T, ?]): Encoder[T] =
    fns.addOne(fn)
    this

  override def encode(obj: T): Any =
    fns.foldLeft(jsonCreator.obj) { (json, fd) =>
      fd.fnApply(obj) match
        case null =>
          if fd.opts.exists(_.opt == NoOmit)
          then json.setByName(fd.name, null)
        case v =>
          val df = fd.opts.flatMap(_.df)
          val pattern = fd.opts.map(_.pattern)
          if df.isEmpty || pattern.isEmpty
          then json.setByName(fd.name, v)
          else json.setByName(fd.name, df.map(_.format(v.asInstanceOf[Date], pattern.get)).get)
      json
    }

  def encodeAsString(obj: T): String =
    encode(obj) match
      case jsonObject: JsonObject =>
        jsonObject.stringify()
      case _ => throw new JsonCodecException(s"can't decode obj $obj")

object Encoder:

  given DataEncoder[String] with
    override def encode(v: String): Any = v

  given DataEncoder[Short] with
    override def encode(v: Short): Any = v

  given DataEncoder[Int] with
    override def encode(v: Int): Any = v

  given DataEncoder[Long] with
    override def encode(v: Long): Any = v

  given DataEncoder[Float] with
    override def encode(v: Float): Any = v

  given DataEncoder[Double] with
    override def encode(v: Double): Any = v

  given DataEncoder[Boolean] with
    override def encode(v: Boolean): Any = v

  given DataEncoder[Date] with
    override def encode(v: Date): Any = v

  given OptionEncoderCodec[T](using encoder: DataEncoder[T]): DataEncoder[Option[T]] with
    override def encode(v: Option[T]): Any = v.orNull

  given SeqEncoderCodec[T](using encoder: DataEncoder[T]): DataEncoder[Seq[T]] with
    override def encode(vs: Seq[T]): Any =
      vs.map(encoder.encode)

  given ListEncoderCodec[T](using encoder: DataEncoder[T]): DataEncoder[List[T]] with
    override def encode(vs: List[T]): Any =
      vs.map(encoder.encode)

  given SetEncoderCodec[T](using encoder: DataEncoder[T]): DataEncoder[Set[T]] with
    override def encode(vs: Set[T]): Any =
      vs.map(encoder.encode)

  inline def typ[T](using JsonCreator): Encoder[T] =
    new Encoder()

  inline def field[T, S](name: String, f: T => S)(encoder: Encoder[T])(using dataEncoder: DataEncoder[S]): Encoder[T] =
    encoder.add(EncoderMedaTada(name, None, dataEncoder, f))
