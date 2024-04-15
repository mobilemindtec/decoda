package br.com.mobilemind.json.codec.infra

import annotations.{JsonType, JsonValue}

import scala.annotation.{MacroAnnotation, StaticAnnotation}
import scala.quoted.*

case class JsonField(
    fieldName: String,
    jsonName: Option[String] = None,
    ignore: Boolean = false,
    omitNull: Boolean = false,
    typ: JsonType = JsonType.Auto
)

object JsonField:
  def apply(fieldName: String, json: JsonValue): JsonField =
    val name = Some(json.name).filter(_.nonEmpty)
    new JsonField(fieldName, name, json.ignore, json.omitNull, json.typ)

case class JsonFields[T](values: List[JsonField]):
  def findField(key: String): Option[JsonField] =
    this.values
      .find(_.fieldName == key)
  // .filterNot(_.ignore)

  def findJsonName(key: String): Option[String] =
    findField(key) match
      case Some(field) =>
        if field.ignore
        then None
        else Some(field.jsonName.getOrElse(key))
      case _ => Some(key)

  // scala to js
  def toNative(key: String, v: Any): Option[Any] =
    findField(key)
      .map { fld =>
        fld.typ match
          case JsonType.IntStr =>
            v match
              case _: Int => v.toString
              case _      => 0
          case JsonType.NumStr =>
            v match
              case _: Double => v.toString
              case _         => 0.0
          case d @ JsonType.DateStr(pattern) =>
            d.dateConverter.format(v, pattern)
          case JsonType.BoolStr =>
            v match
              case b: Boolean => b.toString
              case _          => "false"
          case JsonType.Auto => v
      }

  // js to scala
  def fromNative(key: String, v: Any): Option[Any] =
    findField(key)
      .map { fld =>
        fld.typ match
          case JsonType.IntStr =>
            v match
              case _: Int    => v
              case s: String => s.toIntOption.getOrElse(0)
              case d: Double => d.toInt
              case _         => 0
          case JsonType.NumStr =>
            v match
              case _: Double => v
              case s: String => s.toDoubleOption.getOrElse(0)
              case d: Int    => d.toDouble
              case _         => 0.0
          case d @ JsonType.DateStr(pattern) =>
            v match
              case s: String =>
                d.dateConverter.parse(s, pattern)
              case _ => null
          case JsonType.BoolStr =>
            v match
              case s: String => s == "true"
              case i: Int    => i == 1
              case _         => false
          case JsonType.Auto => v
      }

object JsonFields:
  def apply[T](values: Vector[(String, Any)]): JsonFields[T] =
    new JsonFields(values.map { case (k, v: JsonValue) =>
      JsonField(k, v)
    }.toList)

object macros:

  inline def getJsonFields[T]: JsonFields[T] = ${ getJsonFieldsImpl[T] }

  def getJsonFieldsImpl[T: Type](using q: Quotes): Expr[JsonFields[T]] =
    import quotes.reflect.*
    val annotJson = TypeRepr.of[JsonValue].typeSymbol
    val tuples: Seq[Expr[(String, Any)]] = TypeRepr
      .of[T]
      .typeSymbol
      .primaryConstructor
      .paramSymss
      .flatten
      .collect {
        case sym if sym.hasAnnotation(annotJson) =>
          val fieldNameExpr = Expr(sym.name.asInstanceOf[String])
          val annotExpr = sym.getAnnotation(annotJson).get.asExprOf[JsonValue]
          '{ ($fieldNameExpr, $annotExpr) }
      }
    val seq: Expr[Seq[(String, Any)]] = Expr.ofSeq(tuples)
    '{ JsonFields[T]($seq.toVector) }

  inline def createInstance[T]: T = ${ createInstanceImpl[T] }

  inline def getFieldsDefaults[T]: Map[String, Any] = ${
    getFieldsDefaultsImpl[T]
  }

  // copy from https://github.com/lampepfl/dotty-macro-examples/blob/main/defaultParamsInference/src/macro.scala
  def createInstanceImpl[T: Type](using Quotes): Expr[T] =

    import quotes.*, quotes.reflect.*

    val sym = TypeTree.of[T].symbol
    val comp = sym.companionClass
    val mod = Ref(sym.companionModule)
    val body = comp.tree.asInstanceOf[ClassDef].body
    val idents: List[Ref] =
      for
        case deff @ DefDef(name, _, _, _) <- body
        if name.startsWith("$lessinit$greater$default")
      yield mod.select(deff.symbol)

    Apply(
      Select(
        New(TypeTree.of[T]),
        TypeRepr.of[T].typeSymbol.primaryConstructor
      ),
      idents.map(_.asExpr.asTerm)
    ).asExprOf[T]

  def getFieldsDefaultsImpl[T: Type](using Quotes): Expr[Map[String, Any]] =

    import quotes.*, quotes.reflect.*

    val typ = TypeRepr.of[T]
    val sym = typ.typeSymbol
    val typeArgs = typ.typeArgs
    // val sym = TypeTree.of[T].symbol
    val comp = sym.companionClass
    val mod = Ref(sym.companionModule)

    val names =
      for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
      yield p.name

    val namesExpr: Expr[List[String]] =
      Expr.ofList(names.map(Expr(_)))

    val body = comp.tree.asInstanceOf[ClassDef].body
    val idents: List[Term] =
      for
        case deff @ DefDef(name, _, _, _) <- body
        if name.startsWith("$lessinit$greater$default")
      yield mod.select(deff.symbol).appliedToTypes(typeArgs)

    val identsExpr: Expr[List[Any]] =
      Expr.ofList(idents.map(_.asExpr))

    '{ $namesExpr.zip($identsExpr).toMap }
