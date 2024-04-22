package br.com.mobilemind.json.codec.converter.auto

import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.DateFormatter
import br.com.mobilemind.json.codec.infra.JsonFields
import br.com.mobilemind.json.codec.infra.macros.{
  getFieldsDefaults,
  getJsonFields
}
import br.com.mobilemind.json.codec.parser.Parser
import br.com.mobilemind.json.codec.parser.Parser.AstValue.*
import br.com.mobilemind.json.codec.parser.Parser.{AstProp, AstValue}
import br.com.mobilemind.json.codec.{JsonCodecException, defs}

import java.util.Date
import scala.annotation.implicitNotFound
import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

/** Typeclass for converting between Scala.js and native JavaScript.
  *
  * @tparam A
  *   the type to convert
  */
@implicitNotFound("Could not find an implicit NativeConverter[${A}]")
trait JsonConverter[A]:
  extension (a: A)
    /** Convert type A to a JSON string */
    def toJson: String
    def toAstValue: AstValue

  /** Convert a Json String to type A. Returns either A, or a String error.
    */
  def fromJson(json: Any): A

object JsonConverter:

  inline def apply[A](using nc: JsonConverter[A]): JsonConverter[A] = nc

  private type ImplicitlyAny =
    String | Boolean | Float | Double | Short | Int | Long | Null | Date

  given IntConv: JsonConverter[Int] with
    extension (a: Int)
      override def toAstValue: AstValue = IntLiteral(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Int =
      value match
        case IntLiteral(i)  => i
        case LongLiteral(i) => i.toInt
        case _: Int         => value.asInstanceOf[Int]
        case _: Long        => value.asInstanceOf[Long].toInt
        case _: Short       => value.asInstanceOf[Short].toInt
        case _ => throw new JsonCodecException(s"can't parse ${value} to Int")

  given LongConv: JsonConverter[Long] with
    extension (a: Long)
      override def toAstValue: AstValue = LongLiteral(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Long =
      value match
        case IntLiteral(i)  => i.toLong
        case LongLiteral(i) => i
        case _: Int         => value.asInstanceOf[Int].toLong
        case _: Long        => value.asInstanceOf[Long]
        case _: Short       => value.asInstanceOf[Short].toLong
        case _ => throw new JsonCodecException(s"can't parse ${value} to Int")

  given StringConv: JsonConverter[String] with
    extension (a: String)
      override def toAstValue: AstValue = StringLiteral(a)
      override def toJson: String = a

    override def fromJson(value: Any): String =
      value match
        case StringLiteral(s) => s
        case _ =>
          try value.asInstanceOf[String]
          catch
            case _ =>
              throw new JsonCodecException(s"can't parse ${value} to String")

  given BooleanConv: JsonConverter[Boolean] with

    extension (a: Boolean)
      override def toAstValue: AstValue = BoolLiteral(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Boolean =
      value match
        case BoolLiteral(b) => b
        case _ =>
          try value.asInstanceOf[Boolean]
          catch
            case _ =>
              throw new JsonCodecException(s"can't parse ${value} to Boolean")

  given ShortConv: JsonConverter[Short] with
    extension (a: Short)
      override def toAstValue: AstValue = ShortLiteral(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Short =
      value match
        case IntLiteral(i)   => i.toShort
        case LongLiteral(i)  => i.toShort
        case ShortLiteral(s) => s
        case _: Int          => value.asInstanceOf[Int].toShort
        case _: Long         => value.asInstanceOf[Long].toShort
        case _: Short        => value.asInstanceOf[Short]
        case _ => throw new JsonCodecException(s"can't parse ${value} to Short")

  given DoubleConv: JsonConverter[Double] with
    extension (a: Double)
      override def toAstValue: AstValue = DoubleLiteral(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Double =
      value match
        case DoubleLiteral(d) => d
        case FloatLiteral(f)  => f.toDouble
        case _: Double        => value.asInstanceOf[Double]
        case _: Float         => value.asInstanceOf[Float].toDouble
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Double")

  given FloatConv: JsonConverter[Float] with
    extension (a: Float)
      override def toAstValue: AstValue = FloatLiteral(a)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Float =
      value match
        case FloatLiteral(f)  => f
        case DoubleLiteral(d) => d.toFloat
        case _: Double        => value.asInstanceOf[Double].toFloat
        case _: Float         => value.asInstanceOf[Float]
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Float")

  given NullConv: JsonConverter[Null] with
    extension (a: Null)
      override def toAstValue: AstValue = NullLiteral
      override def toJson: String = "null"

    override def fromJson(value: Any): Null =
      value match
        case NullLiteral => null
        case _ =>
          try value.asInstanceOf[Null]
          catch
            case _ =>
              throw new JsonCodecException(s"can't parse ${value} to Null")

  inline given DateConv: JsonConverter[Date] with
    extension (a: Date)
      override def toAstValue: AstValue = StringLiteral(a.toString)
      override def toJson: String = a.toString

    override def fromJson(value: Any): Date =
      value match
        case _: Date     => value.asInstanceOf[Date]
        case NullLiteral => null.asInstanceOf[Date]
        case null        => null.asInstanceOf[Date]
        case _ =>
          throw new JsonCodecException(s"can't parse ${value} to Date")

  given OptionCodec[A: JsonConverter]: JsonConverter[Option[A]] with
    extension (o: Option[A])
      def toAstValue: AstValue =
        o.map(_.toAstValue).getOrElse(NullLiteral)

      def toJson: String = o.map(_.toJson).orNull

    def fromJson(value: Any): Option[A] =
      value match
        case null | NullLiteral => None
        case _                  => Some(JsonConverter[A].fromJson(value))

  given MapConv[A: JsonConverter]: JsonConverter[Map[String, A]] with
    extension (m: Map[String, A])
      def toAstValue: AstValue =
        val mm = m.map((k, v) => k -> v.asInstanceOf[Any])
        defs.JsonObject(mm).toAstValue

      def toJson: String =
        val mm = m.map((k, v) => k -> v.asInstanceOf[Any])
        defs.JsonObject(mm).stringify()

    def fromJson(value: Any): Map[String, A] =
      value match
        case obj: ObjectAst =>
          objectAstToMap(obj)
        case _ => Map.empty

  given ImmutableMapConv[A: JsonConverter]
      : JsonConverter[immutable.Map[String, A]] with
    extension (m: immutable.Map[String, A])
      def toAstValue: AstValue =
        val mm = m.map((k, v) => k -> v.asInstanceOf[Any])
        defs.JsonObject(mm).toAstValue

      def toJson: String =
        val mm = m.map((k, v) => k -> v.asInstanceOf[Any])
        defs.JsonObject(mm).stringify()

    def fromJson(value: Any): Map[String, A] =
      value match
        case obj: ObjectAst =>
          immutable.Map.from(objectAstToMap(obj))
        case _ => immutable.Map.empty

  given ArrayConv[A: ClassTag: JsonConverter]: JsonConverter[Array[A]] with
    extension (a: Array[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): Array[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, Array.newBuilder)
        case _ => Array.empty

  given IterableConv[A: JsonConverter]: JsonConverter[Iterable[A]] with
    extension (a: Iterable[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): Iterable[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, ArrayBuffer.newBuilder)
        case _ => ArrayBuffer.empty

  given SeqConv[A: JsonConverter]: JsonConverter[Seq[A]] with
    extension (a: Seq[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): Seq[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, Seq.newBuilder)
        case _ => Seq.empty

  given ImmutableSeqConv[A: JsonConverter]: JsonConverter[immutable.Seq[A]] with
    extension (a: immutable.Seq[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): immutable.Seq[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, immutable.Seq.newBuilder)
        case _ => Seq.empty

  given SetCodec[A: JsonConverter]: JsonConverter[mutable.Set[A]] with
    extension (a: mutable.Set[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): mutable.Set[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, mutable.HashSet.newBuilder)
        case _ => mutable.HashSet.empty

  given ImmutableSetCodec[A: JsonConverter]: JsonConverter[Set[A]] with
    extension (a: Set[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): Set[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, immutable.HashSet.newBuilder)
        case _ => immutable.HashSet.empty

  given ListConv[A: JsonConverter]: JsonConverter[List[A]] with
    extension (a: List[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): List[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, List.newBuilder)
        case _ => List.empty

  given VectorConv[A: JsonConverter]: JsonConverter[Vector[A]] with
    extension (a: Vector[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): Vector[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, Vector.newBuilder)
        case _ => Vector.empty

  given BufferConv[A: JsonConverter]: JsonConverter[mutable.Buffer[A]] with
    extension (a: mutable.Buffer[A])
      def toAstValue: AstValue =
        defs.JsonArray(a).toAstValue

      def toJson: String =
        defs.JsonArray(a).stringify()

    def fromJson(value: Any): mutable.Buffer[A] =
      value match
        case arr: ArrayAst =>
          arrayAstToCollection(arr, mutable.Buffer.newBuilder)
        case _ => mutable.Buffer.empty

  private def objectAstToMap[A](obj: ObjectAst)(using
      nc: JsonConverter[A]
  ): Map[String, A] =
    obj.props.map { case AstProp(name, value) =>
      name -> nc.fromJson(value)
    }.toMap

  private def arrayAstToCollection[A, C[_]](
      arr: ArrayAst,
      builder: mutable.Builder[A, C[A]]
  )(using
      nc: JsonConverter[A]
  ): C[A] =
    builder.sizeHint(arr.items.length)
    for it <- arr.items do builder += nc.fromJson(it)
    builder.result()

  /** Derive a NativeConverter for type T. This method is called by the compiler
    * automatically when adding `derives NativeConverter` on a class. You can
    * also use it to derive given instances anywhere, which is useful if
    * Cross-Building a Scala.js project: <br> `given NativeConverter[User] \=
    * NativeConverter.derived` <br> Only Sum and Product types are supported
    */
  inline given derived[A](using
      m: Mirror.Of[A]
  ): JsonConverter[A] =
    type Mets = m.MirroredElemTypes
    type Mels = m.MirroredElemLabels
    type Label = m.MirroredLabel

    val fields = getJsonFields[A]
    val defaults = getFieldsDefaults[A]

    inline m match
      case p: Mirror.ProductOf[A] =>
        new JsonConverter[A]:
          extension (a: A)
            def toJsonObject: defs.JsonObject =
              productToJson[A, Mets, Mels](
                a.asInstanceOf[Product],
                fields = fields,
                jsonObj = defs.JsonObject()
              )

            override def toAstValue: AstValue =
              toJsonObject.toAstValue

            override def toJson: String =
              toJsonObject.stringify()

          override def fromJson(json: Any): A =

            val resArr = Array.ofDim[Any](constValue[Tuple.Size[Mets]])
            json match
              case s: String =>
                Parser.parse(s) match
                  case obj: ObjectAst =>
                    jsonToProduct[A, Mets, Mels](
                      p,
                      resArr,
                      obj,
                      fields = fields,
                      defaults = defaults
                    )
                  case arr: ArrayAst =>
                    throw new JsonCodecException("array value not supported")
                  case astValue: AstValue =>
                    throw new JsonCodecException("ast value not supported")
              case NullLiteral =>
                null.asInstanceOf[A]
              case obj: ObjectAst =>
                jsonToProduct[A, Mets, Mels](
                  p,
                  resArr,
                  obj,
                  fields = fields,
                  defaults = defaults
                )
              case _ =>
                throw new JsonCodecException(s"value ${json} not supported")

      case s: Mirror.SumOf[A] =>
        throw new JsonCodecException("ADT not supported")

  private inline def productToJson[A, Mets, Mels](
      p: Product,
      i: Int = 0,
      jsonObj: defs.JsonObject,
      fields: JsonFields[A]
  ): defs.JsonObject =
    inline (erasedValue[Mets], erasedValue[Mels]) match
      // base case
      case _: (EmptyTuple, EmptyTuple) => jsonObj

      case _: (ImplicitlyAny *: metsTail, mel *: melsTail) =>
        val fieldName = constValue[mel & String]

        fields
          .findJsonName(fieldName)
          .foreach: k =>
            val elem = p.productElement(i)
            val value = fields.toNative(fieldName, elem).getOrElse(elem)
            val fd = fields.findField(fieldName)
            if value == null && fd.nonEmpty
            then
              if !fd.get.omitNull
              then jsonObj.setByName(k, value)
            else jsonObj.setByName(k, value)

        productToJson[A, metsTail, melsTail](p, i + 1, jsonObj, fields)

      case _: (met *: metsTail, mel *: melsTail) =>
        val fieldName = constValue[mel & String]
        val nc = summonInline[JsonConverter[met]]
        val nativeElem = p.productElement(i)
        val omitNull = fields.findField(fieldName).exists(_.omitNull)

        fields
          .findJsonName(fieldName)
          .foreach: k =>
            if nativeElem == null
            then
              if !omitNull then jsonObj.setByName(k, null)
            else
              nc.toAstValue(nativeElem.asInstanceOf[met]) match
                case objAst: ObjectAst =>
                  val value = defs.JsonObject(
                    objAst
                  )
                  jsonObj.setByName(k, value)
                case arr: ArrayAst =>
                  throw new JsonCodecException("array ast value not supported")
                case NullLiteral =>
                  if !omitNull then jsonObj.setByName(k, null)
                case ast =>
                  jsonObj.setByName(k, ast)

        productToJson[A, metsTail, melsTail](p, i + 1, jsonObj, fields)

  private inline def jsonToProduct[A, Mets, Mels](
      mirror: Mirror.ProductOf[A],
      values: Array[Any],
      ast: ObjectAst,
      i: Int = 0,
      fields: JsonFields[A],
      defaults: Map[String, Any]
  ): A =
    inline (erasedValue[Mets], erasedValue[Mels]) match
      case _: (EmptyTuple, EmptyTuple) =>
        mirror.fromProduct(ArrayProduct(values))

      case _: (met *: metsTail, mel *: melsTail) =>
        val nc = summonInline[JsonConverter[met]]
        val key = constValue[mel & String]

        val value =
          fields.findJsonName(key) match
            case None => getDefaultValue(defaults, key)
            case Some(fieldName) =>
              ast.props.find(_.name == fieldName) match
                case None => getDefaultValue(defaults, key)
                case Some(AstProp(_, astValue)) =>
                  astValue match
                    case obj: ObjectAst => nc.fromJson(obj)
                    case arr: ArrayAst  =>
                    case value =>
                      val v = fields.fromNative(key, value).getOrElse(value)
                      nc.fromJson(v)

        values(i) = value

        jsonToProduct[A, metsTail, melsTail](
          mirror,
          values,
          ast,
          i + 1,
          fields,
          defaults
        )

  private def getDefaultValue(defaults: Map[String, Any], key: String): Any =
    defaults.find(_._1 == key) match // get default field value
      case Some(v) => v._2
      case None =>
        throw new JsonCodecException(
          s"can't get default value to field: ${key}"
        )
