
import DecoderItem.SimpleDecoder
import macros.createInstance

import java.util.Date
import scala.collection.mutable

case class DecodeOptions(pattern: String = "",
                         df: Option[DateFormatter] = None)

type DecoderFn[T] = (T, JsonObject, Option[DecodeOptions]) => T

enum DecoderItem:
  case SimpleDecoder[T](v: DecoderFn[T], opts: Option[DecodeOptions] = None)

trait DecoderBase[T]:
  def decode(data: String)(using p: JsonParser): T

  def decode(data: JsonObject): T

class Decoder[T](val options: Option[DecodeOptions] = None)(using factory: () => T) extends DecoderBase[T]:

  var fields = mutable.ListBuffer[DecoderItem]()

  private def empty  = factory()


  private def getJsonValue[S](name: String,
                              json: JsonObject,
                              opts: Option[DecodeOptions],
                              decoder:Option[Decoder[S]]): Option[S] =
    val value = json.getByName(name)
    value match
      case Some(s: String) =>
        val r  = opts match
          case Some(DecodeOptions(patter, Some(df))) =>
            df.parse(s, patter).asInstanceOf[S]
          case _ => s.asInstanceOf[S]
        Some(r)
      case Some(j:JsonObject) =>
        decoder match
          case Some(dec) =>

            Some(dec.decode(j))
          case None => None

      case Some(any) =>
        Some(any.asInstanceOf[S])
      case _ => None


  private def getfn[R](name: String, f: (T, R) => T)(obj: T, json: JsonObject, opts: Option[DecodeOptions] = None): T =
    getJsonValue[R](name, json, opts, None) match
      case Some(v) =>
        f(obj, v)
      case _ => obj

  private def getfnsome[R](name: String, f: (T, Option[R]) => T)(obj: T, json: JsonObject, opts: Option[DecodeOptions] = None): T =
    getJsonValue[R](name, json, opts, None) match
      case None => obj
      case some =>
        f(obj, some)

  def field[S](name: String, f: (T, S) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    fields.addOne(SimpleDecoder(getfn(name, f), opts))
    this

  def ref[S](name: String, f: (T, S) => T, opts: Option[DecodeOptions] = None)(using decoder: Decoder[S]): Decoder[T] =

    val fn: (T, JsonObject, Option[DecodeOptions]) => T =
      case (obj, json, opts) =>
        getJsonValue[S](name, json, opts, Some(decoder)) match
          case Some(v) =>
            f(obj, v)
          case _ => obj


    fields.addOne(SimpleDecoder(fn, opts))
    this

  def string(name: String, f: (T, String) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def int(name: String, f: (T, Int) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def bool(name: String, f: (T, Boolean) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def long(name: String, f: (T, Long) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def float(name: String, f: (T, Float) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def double(name: String, f: (T, Double) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def date(name: String, f: (T, Date) => T, opts: Option[DecodeOptions] = None)(using df: DateFormatter): Decoder[T] =
    field(name, f, opts.map(_.copy(df = Some(df))))

  def list[S](name: String, f: (T, List[S]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    field(name, f, opts)

  def listRef[S](name: String, f: (T, List[S]) => T, opts: Option[DecodeOptions] = None)(using decoder: Decoder[S]): Decoder[T] =
    val fn: (T, JsonObject, Option[DecodeOptions]) => T = {
      case (obj, j, opts) =>
        j.getByName(name) match
          case Some(arr: JsonArray) =>
            val xs = arr.map[S](v => decoder.decode(v.asInstanceOf[JsonObject]))
            f(obj, xs)

          case _ => obj
    }
    fields.addOne(SimpleDecoder(fn, opts))
    this

  def optfield[S](name: String, f: (T, Option[S]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    fields.addOne(SimpleDecoder(getfnsome(name, f), opts))
    this

  def optfRef[S](name: String, f: (T, Option[S]) => T, opts: Option[DecodeOptions] = None)(using decoder: Decoder[S]): Decoder[T] =
    val fn: (T, JsonObject, Option[DecodeOptions]) => T = {
      case (obj, json, opts) =>
        getJsonValue[S](name, json, opts, Some(decoder)) match
          case None => obj
          case some =>
            f(obj, some)
    }
    fields.addOne(SimpleDecoder(fn, opts))
    this

  def optString(name: String, f: (T, Option[String]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f, opts)

  def optInt(name: String, f: (T, Option[Int]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f, opts)

  def optBool(name: String, f: (T, Option[Boolean]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f,opts)

  def optLong(name: String, f: (T, Option[Long]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f, opts)

  def optFloat(name: String, f: (T, Option[Float]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f, opts)

  def optDouble(name: String, f: (T, Option[Double]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f, opts)

  def optDate(name: String, f: (T, Option[Date]) => T, opts: Option[DecodeOptions] = None)(using df: DateFormatter): Decoder[T] =
    optfield(name, f, opts.map(_.copy(df = Some(df))))

  def optList[S](name: String, f: (T, Option[List[S]]) => T, opts: Option[DecodeOptions] = None): Decoder[T] =
    optfield(name, f, opts)

  def optListRef[S](name: String, f: (T, Option[List[S]]) => T, opts: Option[DecodeOptions] = None)(using decoder: Decoder[S]): Decoder[T] =
    val fn: (T, JsonObject, Option[DecodeOptions]) => T = {
      case (obj, j, opts) =>
        j.getByName(name) match
          case Some(arr: JsonArray) =>
            val xs = arr.map[S](v => decoder.decode(v.asInstanceOf[JsonObject]))
            f(obj, Some(xs))

          case _ => obj
    }
    fields.addOne(SimpleDecoder(fn, opts))
    this

  override def decode(s: String)(using p: JsonParser): T =
    decode(p.parse(s))

  override def decode(json: JsonObject): T =
    fields.foldLeft(empty):
      case (obj, SimpleDecoder(op: DecoderFn[T], opts: Option[DecodeOptions])) =>
        op(obj, json, opts)
      case (obj,_) => obj

object Decoder:
  inline def typ[T](options: Option[DecodeOptions] = None): Decoder[T] =
    given creator:  (() => T) = () => createInstance[T]
    Decoder(options)

  inline def typ[T](using j: JsonCreator): Decoder[T] =
    given creator: (() => T) = () => createInstance[T]
    Decoder()


