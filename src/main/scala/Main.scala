
import DecoderItem.SimpleDecoder
import EncoderItem.SimpleEncoder
import OptOmit.{NoOmit, OmitNull}

import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable

trait DateFormatter:
  def parse(date: String, patter: String): Date
  def format(date: Date, patter: String): String

trait JsonObject:
  def getByName(name: String): Any
  def setByName(name: String, value: Any): Unit
  def stringify(): String

trait JsonParser:
  def parse(s: String): JsonObject

trait DataCreator[T]:
    def empty: T

type EncoderFn[T] = (T, JsonObject, Option[EncodeOptions]) => JsonObject

enum OptOmit:
  case OmitNull, NoOmit

case class EncodeOptions(opt: OptOmit = NoOmit,
                         pattern: String = "",
)

enum EncoderItem:
  case SimpleEncoder[T](v: EncoderFn[T], opts: Option[EncodeOptions] = None)

type DecoderFn[T] = (T, JsonObject) => T

enum DecoderItem:
  case SimpleDecoder[T](v: DecoderFn[T])


trait EncoderBase[T]:
  def encode(data: T)(using c: DataCreator[JsonObject]): String
  def encodeAsJson(data: T)(using c: DataCreator[JsonObject]): JsonObject

trait DecoderBase[T]:
  def decode(data: String)(using m: DataCreator[T], p: JsonParser): T
  def decode(data: JsonObject)(using m: DataCreator[T]): T

class Json extends JsonObject:

  val fields: mutable.Map[String, Any] = mutable.Map()

  private def addAll(data: Map[String, Any]): Json =
    fields.addAll(data)
    this

  override def getByName(name: String): Any =
    fields.get(name)

  override def setByName(name: String, value: Any): Unit =
    fields.put(name, value)

  override def stringify(): String =
    val l = fields.map:
      case (k, j: JsonObject) => s"$k: ${j.stringify()}"
      case (k, l: List[_]) =>
        l.headOption match
          case Some(_:JsonObject) =>
            s"$k: [${l.asInstanceOf[List[JsonObject]].map(_.stringify()).mkString(", ")}]"
          case _ =>
            s"$k: [${l.mkString(", ")}]"
      case (k, v) => s"$k: $v"
      
    s"{${l.mkString(", ")}}"



object Json:
  def apply(values: (String, Any)*): Json = new Json().addAll(values.toMap)
  


// encode to json
class Encoder[T](val options: Option[EncodeOptions] = None)(using df: DateFormatter) extends EncoderBase[T]:

  var fields = mutable.ListBuffer[EncoderItem]()

  private def setJsonValue(name: String, json: JsonObject, value: Any, opts: Option[EncodeOptions]) =

    val (v, set) = value match
      case Some(i) =>
        i match
          case d: Date =>
            opts.orElse(options) match
              case Some(EncodeOptions(_, patter)) =>
                if patter.isEmpty
                then (d, true)
                else (df.format(d, patter), true)
              case _ => (i, true)
          case _ => (i, true)
      case None =>
        opts.orElse(options) match
          case Some(EncodeOptions(OmitNull, _)) => (null, false)
          case _ => (null, true)
      case other => (other, true)

    if set
    then json.setByName(name, v)


  private def setfn[S](name: String, f: T => S)(obj:T, json:JsonObject, opts: Option[EncodeOptions]): JsonObject =
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

  def optField[S](name: String, f: T => Option[S], opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

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

  def date(name: String, f: T => Date, opts: Option[EncodeOptions] = None): Encoder[T] =
    field(name, f, opts)

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

  def optDate(name: String, f: T => Option[Date], opts: Option[EncodeOptions] = None): Encoder[T] =
    optField(name, f, opts)

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

  override def encode(obj: T)(using c: DataCreator[JsonObject]): String =
    encodeAsJson(obj).stringify()

  override def encodeAsJson(obj: T)(using c: DataCreator[JsonObject]): JsonObject =
    fields.foldLeft(c.empty):
      case (j, SimpleEncoder(f: EncoderFn[T], opts)) =>
        f(obj, j, opts)
      


// decode from json
class Decoder[T] extends DecoderBase[T]:

  var fields = mutable.ListBuffer[DecoderItem]()

  private def getfn[R](name: String, f: (T, R) => T)(obj: T, json: JsonObject): T =
    json.getByName(name) match
      case Some(v) =>
        f(obj, v.asInstanceOf[R])
      case _ => obj


  def field[S](name: String, f: (T, S) => T): Decoder[T] =
    fields.addOne(SimpleDecoder(getfn(name, f)))
    this

  def ref[S](name: String, f: (T, S) => T)(using dateCreator: DataCreator[S], decoder: Decoder[S]): Decoder[T] =
    
    val fn: (T, JsonObject) => T =
      case (obj, json) =>
        json.getByName(name) match
          case Some(v: JsonObject) =>
            f(obj, decoder.decode(v))
          case _ => obj
    
    
    fields.addOne(SimpleDecoder(fn))
    this

  def string(name: String, f: (T, String) => T): Decoder[T] =
    field(name, f)

  def int(name: String, f: (T, Int) => T): Decoder[T] =
    field(name, f)

  def bool(name: String, f: (T, Boolean) => T): Decoder[T] =
    field(name, f)

  def long(name: String, f: (T, Long) => T): Decoder[T] =
    field(name, f)

  def float(name: String, f: (T, Float) => T): Decoder[T] =
    field(name, f)

  def double(name: String, f: (T, Double) => T): Decoder[T] =
    field(name, f)

  def date(name: String, f: (T, Date) => T): Decoder[T] =
    field(name, f)

  def listOf[S](name: String, f: (T, List[S]) => T): Decoder[T] =
    field(name, f)

  def optString(name: String, f: (T, Option[String]) => T): Decoder[T] =
    field(name, f)

  def optInt(name: String, f: (T, Option[Int]) => T): Decoder[T] =
    field(name, f)

  def optBool(name: String, f: (T, Option[Boolean]) => T): Decoder[T] =
    field(name, f)

  def optLong(name: String, f: (T, Option[Long]) => T): Decoder[T] =
    field(name, f)

  def optFloat(name: String, f: (T, Option[Float]) => T): Decoder[T] =
    field(name, f)

  def optDouble(name: String, f: (T, Option[Double]) => T): Decoder[T] =
    field(name, f)

  def optDate(name: String, f: (T, Option[Date]) => T): Decoder[T] =
    field(name, f)

  def optListOf[S](name: String, f: (T, Option[List[S]]) => T): Decoder[T] =
    field(name, f)

    
  override def decode(s: String)(using m: DataCreator[T], p: JsonParser): T = 
    decode(p.parse(s))

  override def decode(json: JsonObject)(using m: DataCreator[T]): T =
    fields.foldLeft(m.empty): 
      case (obj, SimpleDecoder(op: DecoderFn[T])) =>
        op(obj, json)
      

case class Group(id: Int = 0, description: String = "")
case class Person(id: Int = 0,
                  name: String = "",
                  group: Group,
                  groups: Option[List[Group]] = None,
                  groups2: List[Group] = Nil,
                  birthday: Option[Date] = None,
                  paymentDays: List[Int] = Nil,
                  age: Option[Int] = None)

given JsonParser with
    def parse(s: String): JsonObject = new Json()

given DataCreator[Person] with
  def empty: Person = Person(group = Group())

given DataCreator[Group] with
  def empty: Group = Group()

given DataCreator[JsonObject] with
  def empty: JsonObject = Json()

given DateFormatter with
  override def format(date: Date, patter: String): String = new SimpleDateFormat(patter).format(date)
  override def parse(date: String, patter: String): Date = new SimpleDateFormat(patter).parse(date)

@main def main: Unit =

  given Decoder[Group] = Decoder[Group]
    .string("Description", (p, v) => p.copy(description = v))
    .int("Id", (p, v) => p.copy(id = v))

  val decoder = Decoder[Person]
    .string("Name", (p, v) => p.copy(name = v))
    .int("Id", (p, v) => p.copy(id = v))
    .ref[Group]("Group", (p, v) => p.copy(group = v))


  given Encoder[Group] = Encoder[Group]()
    .int("Id", _.id)
    .string("Description", _.description)

  val encoder = Encoder[Person]()
    .int("Id", _.id)
    .string("Name", _.name)
    .ref("Group", _.group)
    .optDate("Birthday", _.birthday, Some(EncodeOptions(pattern = "yyyy-MM-dd")))
    .optInt("Age", _.age, Some(EncodeOptions(OmitNull)))
    .optListRef("Groups", _.groups)
    .listRef("Groups2", _.groups2)
    .list("PaymentDays", _.paymentDays)


  val p = decoder
    .decode(
      Json("Name" -> "Ricardo", "Id" -> 1, "Group" -> 
        Json("Id" -> 5, "Description" -> "Group 5")))

  val person = Person(
    id = 1,
    name = "Ricardo",
    age = Some(37),
    birthday = Some(new Date()),
    group = Group(id=5, "Group 5"),
    paymentDays = 15 :: 22 :: 28:: Nil,
    groups = Some(Group(id=1, "Group 1") :: Group(id=2, "Group 2") :: Nil),
    groups2 = Group(id=1, "Group 1") :: Group(id=2, "Group 2") :: Nil
  )
  val p1 = encoder.encode(person)

  println(p)
  println(p1)

