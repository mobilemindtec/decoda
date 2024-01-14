## Scala JSON Codec

```scala

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

given JsonCreator with
  def empty: JsonObject = Json()

given DateFormatter with
  override def format(date: Date, patter: String): String = new SimpleDateFormat(patter).format(date)
  override def parse(date: String, patter: String): Date = new SimpleDateFormat(patter).parse(date)

@main def main: Unit =

  given Decoder
[Group
] = Decoder[Group]
  .string("Description", (p, v) => p.copy(description = v))
  .int("Id", (p, v) => p.copy(id = v))

val decoder = Decoder[Person]
  .string("Name", (p, v) => p.copy(name = v))
  .int("Id", (p, v) => p.copy(id = v))
  .ref[Group]("Group", (p, v) => p.copy(group = v))


given Encoder
[Group
] = Encoder[Group]()
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
  group = Group(id = 5, "Group 5"),
  paymentDays = 15 :: 22 :: 28 :: Nil,
  groups = Some(Group(id = 1, "Group 1") :: Group(id = 2, "Group 2") :: Nil),
  groups2 = Group(id = 1, "Group 1") :: Group(id = 2, "Group 2") :: Nil
)
val p1 = encoder.encode(person)

println(p)
println(p1)

```