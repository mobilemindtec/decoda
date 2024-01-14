
import OptOmit.OmitNull
import macros.*

import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable

class Json extends JsonObject:

  val fields: mutable.Map[String, Any] = mutable.Map()

  private def addAll(data: Map[String, Any]): Json =
    fields.addAll(data)
    this

  override def getByName(name: String): Option[Any] =
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

class JArray extends JsonArray:

  private val items = mutable.ListBuffer[Any]()

  override def add(v: Any): Unit = items.addOne(v)

  override def get(i: Int): Any = items(i)

  override def size: Int = items.size

  def addAll(data: Seq[Any]): JArray =
    items.addAll(data)
    this

object JArray:
  def apply(vs: Any*): JArray = new JArray().addAll(vs)

object Json:
  def apply(values: (String, Any)*): Json = new Json().addAll(values.toMap)


case class Group(id: Int = 0, description: String = "")
case class Person(id: Int = 0,
                  name: String = "",
                  group: Group = null,
                  groups: Option[List[Group]] = None,
                  groups2: List[Group] = Nil,
                  birthday: Option[Date] = None,
                  paymentDays: List[Int] = Nil,
                  age: Option[Int] = None)

given JsonParser with
    def parse(s: String): JsonObject = new Json()

given JsonCreator with
  def empty: JsonObject = Json()

given DateFormatter with
  override def format(date: Date, patter: String): String = new SimpleDateFormat(patter).format(date)
  override def parse(date: String, patter: String): Date = new SimpleDateFormat(patter).parse(date)



@main def main: Unit =

  given Decoder[Group] = Decoder.typ[Group]
    .string("Description", (p, v) => p.copy(description = v))
    .int("Id", (p, v) => p.copy(id = v))

  val decoder = Decoder.typ[Person]
    .string("Name", (p, v) => p.copy(name = v))
    .int("Id", (p, v) => p.copy(id = v))
    .ref[Group]("Group", (p, v) => p.copy(group = v))
    .optDate("Birthday", (p, v) => p.copy(birthday = v), Some(DecodeOptions(pattern = "yyyy-MM-dd")))
    .optInt("Age", (p, v) => p.copy(age = v))
    .optListRef[Group]("Groups", (p, v) => p.copy(groups = v))
    .listRef[Group]("Groups2", (p, v) => p.copy(groups2 = v))
    .list[Int]("PaymentDays", (p, v) => p.copy(paymentDays = v))


  given Encoder[Group] = Encoder.typ[Group]
    .int("Id", _.id)
    .string("Description", _.description)

  val encoder = Encoder.typ[Person]
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
      Json(
        "Name" -> "Ricardo",
        "Id" -> 1,
        "Group" -> Json("Id" -> 5, "Description" -> "Group 5"),
        "Birthday" -> "2010-01-05",
        "PaymentDays" -> List(15,20, 25),
        "Groups" -> JArray(
          Json("Id" -> 1, "Description" -> "Group 1"),
          Json("Id" -> 2, "Description" -> "Group 2")
        ),
        "Groups2" -> JArray(
          Json("Id" -> 3, "Description" -> "Group 3"),
          Json("Id" -> 4, "Description" -> "Group 4")
        )))

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

  println(s"decoded -> $p")
  println(s"encoded -> $p1")


