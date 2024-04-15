package codec

import br.com.mobilemind.json.codec.infra.annotations.JsonValue
import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.JsonCreator
import br.com.mobilemind.json.codec.converter.auto.converter.JsonConverter
import br.com.mobilemind.json.codec.defs
import org.scalatest.funsuite.AnyFunSuite



class JsonConverterTest extends AnyFunSuite:

  given JsonCreator with
    override def empty: base.JsonObject = new defs.JsonObject()


  test("native converter test"){

    case class Person(@JsonValue id: Int = 0, @JsonValue name: String) derives JsonConverter

    val person = Person(name = "Ricardo")
    val json = person.toJson
    println(json)
    val p = JsonConverter[Person].fromJson(json)
    println(p)

  }