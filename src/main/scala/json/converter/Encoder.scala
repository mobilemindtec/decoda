package json.converter

import EncoderItem.SimpleEncoder
import OptOmit.{NoOmit, OmitNull}
import base._

import java.util.Date
import scala.collection.mutable

type EncoderFn[T] = (T, JsonObject, Option[EncodeOptions]) => JsonObject

case class EncodeOptions(opt: OptOmit = NoOmit,
                         pattern: String = "",
                         df: Option[DateFormatter] = None,
                        )

trait EncoderBase[T]:
  def encode(data: T): String
  def encodeAsJson(data: T): JsonObject

enum EncoderItem:
  case SimpleEncoder[T](v: EncoderFn[T], opts: Option[EncodeOptions] = None)

class Encoder[T](options: Option[EncodeOptions] = None)(using jsonCreator: JsonCreator) extends EncoderBase[T]:

  var fields = mutable.ListBuffer[EncoderItem]()

  private def setJsonValue(name: String, json: JsonObject, value: Any, opts: Option[EncodeOptions]) =

    val (v, set) = value match
      case Some(i) =>
        i match
          case d: Date =>
            opts.orElse(options) match
              case Some(EncodeOptions(_, patter, Some(df))) =>
                if patter.isEmpty
                then (d, true)
                else (df.format(d, patter), true)
              case _ => (i, true)
          case _ => (i, true)
      case None =>
        opts.orElse(options) match
          case Some(EncodeOptions(OmitNull, _,_)) => (null, false)
          case _ => (null, true)
      case other => (other, true)

    if set
    then json.setByName(name, v)


  private def setfn[S](name: String, f: T => S)(obj:T, json: JsonObject, opts: Option[EncodeOptions]): JsonObject =
    setJsonValue(name, json, f(obj), opts)
    json

  def field[S](name: String, f: T => S, opts: Option[EncodeOptions] = None): Encoder[T] =
    fields.addOne(SimpleEncoder(setfn(name, f), opts))
    this

  def ref[S](name: String, f: T => S, opts: Option[EncodeOptions] = None)(using encoder: Encoder[S]) : Encoder[T] =
    val fn: (T, JsonObject, Option[EncodeOptions]) => JsonObject = {
      case (obj, json, opts) =>
        setJsonValue(name, json, encoder.encodeAsJson(f(obj)), opts)
        json
    }
    fields.addOne(SimpleEncoder(fn, opts))
    this
  
  def string(name: String, f: T => String, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def int(name: String, f: T => Int, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def bool(name: String, f: T => Boolean, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def long(name: String, f: T => Long, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def float(name: String, f: T => Float, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def double(name: String, f: T => Double, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def date(name: String, f: T => Date, opts: Option[EncodeOptions] = None)(using df: DateFormatter): Encoder[T] =
    field(name, f, opts.map(_.copy(df = Some(df))))

  def list[S](name: String, f: T => List[S], opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def listRef[S](name: String, f: T => List[S], opts: Option[EncodeOptions] = None)(using encoder: Encoder[S]): Encoder[T] =
    val fn: (T, JsonObject, Option[EncodeOptions]) => JsonObject = {
      case (obj, json, opts) =>
        val array = f(obj).map(encoder.encodeAsJson)
        setJsonValue(name, json, array, opts)
        json
    }
    fields.addOne(SimpleEncoder(fn, opts))
    this

  def optField[S](name: String, f: T => Option[S], opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

  def optRef[S](name: String, f: T => Option[S], opts: Option[EncodeOptions] = None)(using encoder: Encoder[S]): Encoder[T] =
    val fn: (T, JsonObject, Option[EncodeOptions]) => JsonObject = {
      case (obj, json, opts) =>
        val opt = f(obj).map(encoder.encodeAsJson)
        setJsonValue(name, json, opt, opts)
        json
    }
    fields.addOne(SimpleEncoder(fn, opts))
    this

  def optString(name: String, f: T => Option[String], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optInt(name: String, f: T => Option[Int], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optBool(name: String, f: T => Option[Boolean], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optLong(name: String, f: T => Option[Long], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optFloat(name: String, f: T => Option[Float], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optDouble(name: String, f: T => Option[Double], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optDate(name: String, f: T => Option[Date], opts: Option[EncodeOptions] = None)(using df: DateFormatter): Encoder[T] =
    optField(name, f, opts.map(_.copy(df = Some(df))))

  def optList[S](name: String, f: T => Option[List[S]], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

  def optListRef[S](name: String, f: T => Option[List[S]], opts: Option[EncodeOptions] = None)(using encoder: Encoder[S]): Encoder[T] =
    val fn: (T, JsonObject, Option[EncodeOptions]) => JsonObject = {
      case (obj, json, opts) =>
        val array = f(obj).map(_.map(encoder.encodeAsJson))
        setJsonValue(name, json, array, opts)
        json
    }
    fields.addOne(SimpleEncoder(fn, opts))
    this

  override def encode(obj: T): String =
    encodeAsJson(obj).stringify()

  override def encodeAsJson(obj: T): JsonObject =
    fields.foldLeft(jsonCreator.empty):
      case (j, SimpleEncoder(f: EncoderFn[T], opts)) =>
        f(obj, j, opts)
      case (j, _) => j


object Encoder:

  inline def typ[T](options: Option[EncodeOptions] = None)(using JsonCreator): Encoder[T] =
    Encoder(options)

  inline def typ[T](using j: JsonCreator): Encoder[T] =
    Encoder()
