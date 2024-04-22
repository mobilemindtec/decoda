package br.com.mobilemind.json.codec

import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.{Json, JsonValue}
import br.com.mobilemind.json.codec.parser.Parser
import br.com.mobilemind.json.codec.parser.Parser.AstValue.*
import br.com.mobilemind.json.codec.parser.Parser.{AstProp, AstValue}

import scala.collection.mutable

object defs:

  object Json:
    def parser(json: String): Json =
      Parser.parse(json) match
        case obj: ObjectAst => new JsonObject(obj.props)
        case arr: ArrayAst  => new JsonArray(arr.items)
        case ast            => JsonValue(extractAstValue(ast))

  private def selectAstType(value: Any): AstValue =
    value match
      case s: AstValue    => s
      case s: String      => StringLiteral(s)
      case s: Int         => IntLiteral(s)
      case s: Long        => LongLiteral(s)
      case s: Float       => FloatLiteral(s)
      case s: Double      => DoubleLiteral(s)
      case s: Boolean     => BoolLiteral(s)
      case s: JsonObject  => ObjectAst(s.getProps)
      case s: JsonArray   => ArrayAst(s.getItems)
      case s: Iterable[?] => ArrayAst(s.map(selectAstType).toArray)
      case null           => NullLiteral
      case _ => NullLiteral // throw new InvalidParameterException(s"$value")

  private def extractAstValue(value: AstValue): Any =
    value match
      case StringLiteral(s) => s
      case IntLiteral(s)    => s
      case LongLiteral(s)   => s
      case FloatLiteral(s)  => s
      case DoubleLiteral(s) => s
      case BoolLiteral(s)   => s
      case ObjectAst(props) => new JsonObject(props)
      case ArrayAst(items)  => new JsonArray(items)
      case NullLiteral      => null

  class JsonArray(arrayAst: Seq[AstValue] = Nil) extends base.JsonArray:
    private val items = mutable.ArrayBuffer.from(arrayAst)

    def getItems: Array[AstValue] = items.toArray

    def toAstValue: AstValue = ArrayAst(getItems)

    override def get(i: Int): Any =
      extractAstValue(items(i))

    override def size: Int = items.size

    override def add(v: Any): Unit =
      v match
        case ast: AstValue => items.append(ast)
        case _             => items.append(selectAstType(v))

    override def stringify(): String =
      Parser.format(toAstValue)

  object JsonArray:
    def apply(items: Any*): JsonArray =
      new JsonArray(items.map(selectAstType))

    def apply(items: collection.Iterable[?]): JsonArray =
      new JsonArray(items.map(selectAstType).toSeq)

  class JsonObject(astProps: Seq[AstProp] = Nil) extends base.JsonObject:

    private val props = mutable.ListBuffer.from(astProps)

    def getProps: Seq[AstProp] = props.toSeq

    def toAstValue: AstValue = ObjectAst(getProps)

    // return AstValue
    override def getByName(name: String): Option[Any] =
      props.find(_.name == name).map(_.value).map(extractAstValue)

    override def setByName(name: String, value: Any): Unit =
      props.append(AstProp(name, selectAstType(value)))

    override def stringify(): String =
      Parser.format(toAstValue)

  object JsonObject:

    def apply(values: (String, Any)*): JsonObject =
      val props = values.map((k, v) => AstProp(k, selectAstType(v)))
      new JsonObject(props)

    def apply(values: Map[String, Any]): JsonObject =
      val props = values.map((k, v) => AstProp(k, selectAstType(v)))
      new JsonObject(props.toSeq)

    def apply(objAst: ObjectAst): JsonObject = new JsonObject(
      objAst.props
    )
