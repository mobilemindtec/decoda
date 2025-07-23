package io.decoda

import decoda.*
import decoda.Encoder.{field, typ, given}
import io.decoda.converter.base
import org.scalatest.funsuite.AnyFunSuite
import java.text.SimpleDateFormat
import java.util.Date

class EncoderTest extends AnyFunSuite:

  given JsonCreator:
    override def mkObject: base.JsonObject = defs.JsonObject()
    override def mkArray: base.JsonArray = defs.JsonArray()

  class DateFormatImpl extends DateFormatter:
    override def format(date: Date, pattern: String): String =
      new SimpleDateFormat(pattern).format(date)

    override def parse(date: String, pattern: String): Date =
      new SimpleDateFormat(pattern).parse(date)

  test("encoder person test") {

    case class Group(id: Int = 0, description: String = "")
    case class Person(id: Int = 0, name: String = "", group: Option[Group] = None)

    given Encoder[Group] =
      typ[Group]
        |> field("id", _.id)
        |> field("description", _.description)

    val personEncoder =
      typ[Person]
        |> field("id", _.id)
        |> field("name", _.name)
        |> field("group", _.group)

    val jsonStr =
      personEncoder.encodeObject(Person(1, "Ricardo", Some(Group(10, "g10"))))

    val expected = """{"id": 1, "name": "Ricardo", "group": {"id": 10, "description": "g10"}}"""

    assert(jsonStr == expected)

  }

  // Teste 1: Person com tipos de dados variados
  test("Person with various data types") {
    // Classe relacionada
    case class Address(city: String, state: String, country: String, zipCode: Int)

    // Classe principal
    case class Person(
        id: Int,
        name: String,
        age: Int,
        isMarried: Boolean,
        salary: Double,
        hobbies: List[String],
        birthDate: String,
        address: Address,
        phoneNumber: String,
        email: String
    )

    // Configuração do encoder para Address
    given Encoder[Address] =
      typ[Address]
        |> field("city", _.city)
        |> field("state", _.state)
        |> field("country", _.country)
        |> field("zipCode", _.zipCode)

    // Configuração do encoder para Person
    val personEncoder =
      typ[Person]
        |> field("id", _.id)
        |> field("name", _.name)
        |> field("age", _.age)
        |> field("isMarried", _.isMarried)
        |> field("salary", _.salary)
        |> field("hobbies", _.hobbies)
        |> field("birthDate", _.birthDate)
        |> field("address", _.address)
        |> field("phoneNumber", _.phoneNumber)
        |> field("email", _.email)

    // Dados de teste
    val person = Person(
      1,
      "John Doe",
      35,
      true,
      75000.0,
      List("Reading", "Cycling"),
      "1988-05-10",
      Address("New York", "NY", "USA", 10001),
      "123-456-7890",
      "john@example.com"
    )

    // Codificação e validação
    val jsonStr = personEncoder.encodeObject(person)

    val expected =
      """{"id": 1, "name": "John Doe", "age": 35, "isMarried": true, "salary": 75000.00, "hobbies": ["Reading", "Cycling"], "birthDate": "1988-05-10", "address": {"city": "New York", "state": "NY", "country": "USA", "zipCode": 10001}, "phoneNumber": "123-456-7890", "email": "john@example.com"}"""

    assert(jsonStr == expected)
  }

  // Teste 2: Employee com tipos de dados variados
  test("Employee with various data types") {

    val encodeDateOpts = EncodeOptions(pattern = "yyyy-MM-dd", df = Some(new DateFormatImpl))

    // Classe relacionada
    case class Department(name: String, code: String, location: String)

    // Classe principal
    case class Employee(
        id: Int,
        firstName: String,
        lastName: String,
        age: Int,
        isActive: Boolean,
        salary: Double,
        hireDate: Date,
        department: Department,
        projects: List[String],
        officePhone: String,
        personalEmail: String
    )

    // Configuração do encoder para Department
    given Encoder[Department] =
      typ[Department]
        |> field("name", _.name)
        |> field("code", _.code)
        |> field("location", _.location)

    // Configuração do encoder para Employee
    val employeeEncoder =
      typ[Employee]
        |> field("id", _.id)
        |> field("firstName", _.firstName)
        |> field("lastName", _.lastName)
        |> field("age", _.age)
        |> field("isActive", _.isActive)
        |> field("salary", _.salary)
        |> field("hireDate", _.hireDate, encodeDateOpts)
        |> field("department", _.department)
        |> field("projects", _.projects)
        |> field("officePhone", _.officePhone)
        |> field("personalEmail", _.personalEmail)

    // Dados de teste
    val employee = Employee(
      2,
      "Jane",
      "Smith",
      28,
      true,
      82000.0,
      new DateFormatImpl().parse("2020-03-15", "yyyy-MM-dd"),
      Department("Engineering", "ENG", "San Francisco"),
      List("Project A", "Project B"),
      "987-654-3210",
      "jane.smith@example.com"
    )

    // Codificação e validação
    val jsonStr = employeeEncoder.encodeObject(employee)
    val expected =
      """{"id": 2, "firstName": "Jane", "lastName": "Smith", "age": 28, "isActive": true, "salary": 82000.00, "hireDate": "2020-03-15", "department": {"name": "Engineering", "code": "ENG", "location": "San Francisco"}, "projects": ["Project A", "Project B"], "officePhone": "987-654-3210", "personalEmail": "jane.smith@example.com"}"""

    assert(jsonStr == expected)
  }

  test("encode all types test") {
    case class Group(id: Int = 0, description: String = "")
    case class Person(
        id: Int = 0,
        name: String = "",
        group: Group = Group(),
        gopt: Option[Group] = None,
        gopt1: Option[Group] = None,
        ints: List[Int] = Nil,
        shorts: List[Short] = Nil,
        longs: List[Long] = Nil,
        strings: List[String] = Nil,
        doubles: List[Double] = Nil,
        floats: List[Float] = Nil,
        groups: List[Group] = Nil,
        groupsOpts: Option[List[Group]] = None,
        groupsOpts2: Option[List[Group]] = None
    )

    given Encoder[Group] =
      typ[Group]
        |> field("id", _.id)
        |> field("description", _.description)

    val encoder =
      typ[Person]
        |> field("id", _.id)
        |> field("name", _.name)
        |> field("group", _.group)
        |> field("gopt", _.gopt)
        |> field("gopt1", _.gopt1)
        |> field("ints", _.ints)
        |> field("longs", _.longs)
        |> field("shorts", _.shorts)
        |> field("doubles", _.doubles)
        |> field("floats", _.floats)
        |> field("strings", _.strings)
        |> field("groups", _.groups)
        |> field("groupsOpts2", _.groupsOpts2)

    val person =
      Person(
        1,
        "Ricardo",
        Group(10, "g10"),
        None,
        Some(Group(11, "g11")),
        List(1, 2, 3),
        List[Short](7, 8, 9),
        List(4L, 5L, 6L),
        List("aa", "bb", "cc"),
        List(1.1d, 1.2d),
        List(1.3f, 1.4f),
        List(Group(12, "g12"), Group(13, "g13")),
        None,
        Some(List(Group(14, "g14"), Group(15, "g15")))
      )
    val jsonStr = encoder.encodeObject(person)

    val expected =
      """{"id": 1, "name": "Ricardo", "group": {"id": 10, "description": "g10"}, "gopt1": {"id": 11, "description": "g11"}, "ints": [1, 2, 3], "longs": [4, 5, 6], "shorts": [7, 8, 9], "doubles": [1.10, 1.20], "floats": [1.30, 1.40], "strings": ["aa", "bb", "cc"], "groups": [{"id": 12, "description": "g12"}, {"id": 13, "description": "g13"}], "groupsOpts2": [{"id": 14, "description": "g14"}, {"id": 15, "description": "g15"}]}"""
    assert(jsonStr == expected)
  }
