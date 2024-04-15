package br.com.mobilemind.json.codec

import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.{Json, JsonValue}
import br.com.mobilemind.json.codec.parser.Json.{AstProp, AstValue}
import br.com.mobilemind.json.codec.parser.Json.AstValue.*
import br.com.mobilemind.json.codec.parser.Json

import java.security.InvalidParameterException
import java.util
import scala.collection.mutable

object defs:

  private def selectAstType(value: Any): AstValue =
    value match
      case s: String => StringLiteral(s)
      case s: Int => IntLiteral(s)
      case s: Long => LongLiteral(s)
      case s: Float => FloatLiteral(s)
      case s: Double => DoubleLiteral(s)
      case s: Boolean => BoolLiteral(s)
      case s: JsonObject => ObjectAst(s.getProps)
      case s: JsonArray => ArrayAst(s.getItems)
      case s: Iterable[?] => ArrayAst(s.map(selectAstType).toArray)
      case null => NullLiteral
      case _ => NullLiteral //throw new InvalidParameterException(s"$value")

  private def extractAstValue(value: AstValue): Any =
    value match
      case StringLiteral(s) => s
      case IntLiteral(s) => s
      case LongLiteral(s) => s
      case FloatLiteral(s) => s
      case DoubleLiteral(s) => s
      case BoolLiteral(s) => s
      case ObjectAst(props) => new JsonObject(props)
      case ArrayAst(items) => new JsonArray(items)
      case NullLiteral => null

  class JsonArray(arrayAst: Seq[AstValue] = Nil) extends base.JsonArray:
    private val items = mutable.ArrayBuffer.from(arrayAst)
    
    def getItems: Array[AstValue] = items.toArray

    override def get(i: Int): Any =
      extractAstValue(items(i))

    override def size: Int = items.size

    override def add(v: Any): Unit =
      v match
        case ast: AstValue => items.append(ast)
        case _ => items.append(selectAstType(v))

  object JsonArray:
    def apply(items: Any*): JsonArray =
      new JsonArray(items.map(selectAstType))

  class JsonObject(astProps: Seq[AstProp] = Nil) extends base.JsonObject:

    private val props = mutable.ListBuffer.from(astProps)

    def getProps: Seq[AstProp] = props.toSeq

    // return AstValue
    override def getByName(name: String): Option[Any] =
      props.find(_.name==name).map(_.value).map(extractAstValue)

    override def setByName(name: String, value: Any): Unit =

      props.append(AstProp(name, selectAstType(value)))

    override def stringify(): String =
      Json.format(ObjectAst(props.toSeq))

    override def parser(json: String): Json =
      Json.parse(json) match
        case obj: ObjectAst => new JsonObject(obj.props)
        case arr: ArrayAst => new JsonArray(arr.items)
        case ast => JsonValue(extractAstValue(ast))

  object JsonObject:
    def apply(values: (String, Any)*): JsonObject =
      val props = values.map((k, v) => AstProp(k, selectAstType(v)))
      new JsonObject(props)
