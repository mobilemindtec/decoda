package codec

import br.com.mobilemind.json.codec.parser.Json
import br.com.mobilemind.json.codec.parser.Json.AstValue.*
import br.com.mobilemind.json.codec.parser.Json.{AstProp, AstValue}
import org.scalatest.funsuite.AnyFunSuite

class JsonParserTest extends AnyFunSuite:

  test("json parse"){
    val json =
      """
        |{
        |"name":  "Ricardo Bocchi",
        |"age":  38,
        |"salary":  1260.00,
        |"employer":  { "company":  "Mobile Mind", "enabled":  true }
        |}""".stripMargin

    Json.parse(json) match
      case ObjectAst(props) =>

        assert(props.contains(AstProp("name", StringLiteral("Ricardo Bocchi"))))
        assert(props.contains(AstProp("age", LongLiteral(38))))
        assert(props.contains(AstProp("salary", DoubleLiteral(1260))))

        val empProps = AstProp("company", StringLiteral("Mobile Mind")) :: AstProp("enabled", BoolLiteral(true)) :: Nil

        assert(props.contains(AstProp("employer", ObjectAst(empProps))))

      case thing =>  fail(s"expect ObjectAst, but receive ${thing}")
  }

  test("json format") {
    val json =
      """{"name": "Ricardo Bocchi", "age": 38, "salary": 1260.00, "employer": {"company": "Mobile Mind", "enabled": true}}"""

    Json.parse(json) match
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but receive ${thing}")
  }

  // 1. Complex object with nested object and array
  test("complex object with nested object and array") {
    val json =
      """{"name": "John", "age": 30, "address": {"city": "New York", "zipcode": 10001}, "tags": ["tag1", "tag2"], "status": true, "balance": 100.50}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 2. Array of complex objects
  test("array of complex objects") {
    val json =
      """[{"name": "John", "age": 30}, {"name": "Alice", "age": 28}]"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 3. Object with boolean, string, integer, and float values
  test("object with boolean, string, integer, and float values") {
    val json =
      """{"isActive": true, "name": "Alice", "age": 28, "balance": 1000.75}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 4. Nested object with nested arrays
  test("nested object with nested arrays") {
    val json =
      """{"name": "Bob", "address": {"city": "Los Angeles", "zipcode": 90001}, "contacts": [{"type": "email", "value": "bob@example.com"}, {"type": "phone", "value": "123-456-7890"}]}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 5. Object with empty array
  test("object with empty array") {
    val json =
      """{"name": "Jane", "age": 35, "hobbies": []}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 6. Array of objects with mixed data types
  test("array of objects with mixed data types") {
    val json =
      """[{"name": "John", "age": 30, "isActive": true}, {"name": "Alice", "age": 28, "isActive": false}]"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 7. Object with nested objects containing arrays
  test("object with nested objects containing arrays") {
    val json =
      """{"name": "Charlie", "details": {"languages": ["English", "Spanish"], "addresses": [{"city": "New York", "zipcode": 10001}, {"city": "Los Angeles", "zipcode": 90001}]}}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 8. Object with null values
  test("object with null values") {
    val json =
      """{"name": null, "age": null, "balance": null}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 9. Object with nested objects and arrays containing null values
  test("object with nested objects and arrays containing null values") {
    val json =
      """{"name": "David", "details": {"languages": ["English", null, "Spanish"], "addresses": [{"city": "New York", "zipcode": 10001}, null]}}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 10. Array of mixed data types
  test("array of mixed data types") {
    val json =
      """["John", 30, true, {"city": "New York", "zipcode": 10001}]"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // Test cases with larger and more complex JSONs

  // 1. Large object with nested arrays and objects
  test("large object with nested arrays and objects") {
    val json = """{"name": "John", "age": 30, "address": {"city": "New York", "zipcode": 10001}, "tags": ["tag1", "tag2"], "contacts": [{"type": "email", "value": "john@example.com"}, {"type": "phone", "value": "123-456-7890"}], "details": {"education": [{"degree": "Bachelor", "university": "XYZ University", "year": 2010}, {"degree": "Master", "university": "ABC University", "year": 2015}], "employment": [{"company": "Tech Corp", "position": "Software Engineer", "years": 5}, {"company": "Data Corp", "position": "Data Analyst", "years": 3}]}}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 2. Large array of complex objects
  test("large array of complex objects") {
    val json = """[{"name": "John", "age": 30}, {"name": "Alice", "age": 28}, {"name": "Bob", "age": 35}, {"name": "Emma", "age": 32}, {"name": "Michael", "age": 40}]"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 3. Object with deeply nested structure
  test("object with deeply nested structure") {
    val json = """{"name": "Alice", "details": {"personal": {"dob": "1990-05-15", "gender": "female"}, "address": {"city": "New York", "zipcode": 10001}, "education": [{"degree": "Bachelor", "university": "ABC University", "year": 2012}, {"degree": "Master", "university": "XYZ University", "year": 2015}]}}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 4. Object with deeply nested arrays
  test("object with deeply nested arrays") {
    val json = """{"name": "Bob", "contacts": [{"type": "email", "details": [{"email": "bob@example.com", "verified": true}, {"email": "bob2@example.com", "verified": false}]}, {"type": "phone", "details": [{"phone": "123-456-7890", "verified": true}, {"phone": "987-654-3210", "verified": true}]}]}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }

  // 5. Object with multiple levels of nesting
  test("object with multiple levels of nesting") {
    val json = """{"name": "Charlie", "details": {"personal": {"dob": "1985-10-20", "gender": "male"}, "address": {"city": "Los Angeles", "zipcode": 90001}, "employment": [{"company": "Tech Corp", "position": "Software Engineer", "years": 7}, {"company": "Finance Corp", "position": "Financial Analyst", "years": 4}], "contacts": [{"type": "email", "details": [{"email": "charlie@example.com", "verified": true}]}, {"type": "phone", "details": [{"phone": "456-789-0123", "verified": true}]}]}}"""

    Json.parse(json) match {
      case ast: AstValue =>
        assert(Json.format(ast) == json, "json not equals")
      case thing => fail(s"expect AstValue, but received $thing")
    }
  }
