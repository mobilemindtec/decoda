package br.com.mobilemind.json.codec.converter.auto

import br.com.mobilemind.json.codec.JsonCodecException
import br.com.mobilemind.json.codec.converter.auto.ArrayProduct
import br.com.mobilemind.json.codec.converter.base
import br.com.mobilemind.json.codec.converter.base.{JsonArray, JsonCreator, JsonObject}
import br.com.mobilemind.json.codec.infra.macros.{getFieldsDefaults, getJsonFields}

import java.util.Date
import scala.annotation.implicitNotFound
import scala.collection.{immutable, mutable}
import scala.compiletime.*
import scala.deriving.Mirror
import scala.reflect.ClassTag

object converter:

  trait JsonValueConverter[A]:
    def convert(value: Any): A

  object JsonValueConverter:

    given IntConv: JsonValueConverter[Int] with
      override def convert(value: Any): Int =
        value match
          case _: Int   => value.asInstanceOf[Int]
          case _: Long  => value.asInstanceOf[Long].toInt
          case _: Short => value.asInstanceOf[Short].toInt
          case _ => throw new JsonCodecException(s"can't parse ${value} to Int")

    given LongConv: JsonValueConverter[Long] with
      override def convert(value: Any): Long =
        value match
          case _: Int   => value.asInstanceOf[Int].toLong
          case _: Long  => value.asInstanceOf[Long]
          case _: Short => value.asInstanceOf[Short].toLong
          case _ => throw new JsonCodecException(s"can't parse ${value} to Int")

    given StringConv: JsonValueConverter[String] with
      override def convert(value: Any): String =
        try value.asInstanceOf[String]
        catch
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to String")

    given BooleanConv: JsonValueConverter[Boolean] with
      override def convert(value: Any): Boolean =
        try value.asInstanceOf[Boolean]
        catch
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to Boolean")

    given ShortConv: JsonValueConverter[Short] with
      override def convert(value: Any): Short =
        value match
          case _: Short => value.asInstanceOf[Short]
          case _: Int   => value.asInstanceOf[Int].toShort
          case _: Long  => value.asInstanceOf[Long].toShort
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to Short")

    given DoubleConv: JsonValueConverter[Double] with
      override def convert(value: Any): Double =
        value match
          case _: Double => value.asInstanceOf[Double]
          case _: Float  => value.asInstanceOf[Float].toDouble
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to Double")

    given FloatConv: JsonValueConverter[Float] with
      override def convert(value: Any): Float =
        value match
          case _: Double => value.asInstanceOf[Double].toFloat
          case _: Float  => value.asInstanceOf[Float]
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to Float")

    given NullConv: JsonValueConverter[Null] with
      def convert(value: Any): Null =
        try value.asInstanceOf[Null]
        catch
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to Null")

    given DateConv: JsonValueConverter[Date] with
      def convert(value: Any): Date =
        try
          value match
            case _: Date => value.asInstanceOf[Date]
            case l: Long => new Date(l)
            case l: Int  => new Date(l.asInstanceOf[Long])
        catch
          case _ =>
            throw new JsonCodecException(s"can't parse ${value} to Date")

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

    /** Convert a Json String to type A. Returns either A, or a String error.
      */
    def fromJson(json: String): A

  object JsonConverter:

    inline def apply[A](using nc: JsonConverter[A]): JsonConverter[A] = nc

    private type ImplicitlyAny = String | Boolean | Byte | Short | Int | Float |
      Double | Null

    /** Derive a NativeConverter for type T. This method is called by the
      * compiler automatically when adding `derives NativeConverter` on a class.
      * You can also use it to derive given instances anywhere, which is useful
      * if Cross-Building a Scala.js project: <br> `given NativeConverter[User]
      * \= NativeConverter.derived` <br> Only Sum and Product types are
      * supported
      */
    inline given derived[A](using
        m: Mirror.Of[A],
        jsonCreator: JsonCreator
    ): JsonConverter[A] =
      type Mets = m.MirroredElemTypes
      type Mels = m.MirroredElemLabels
      type Label = m.MirroredLabel

      inline m match
        case p: Mirror.ProductOf[A] =>
          new JsonConverter[A]:
            extension (a: A)
              def toJson: String =
                val jsonObj = productToJson[A, Mets, Mels](
                  a.asInstanceOf[Product],
                  jsonObj = jsonCreator.empty
                )
                jsonObj.stringify()

            def fromJson(jsonStr: String): A =
              jsonCreator.empty.parser(jsonStr) match
                case obj: JsonObject =>
                  val resArr = Array.ofDim[Any](constValue[Tuple.Size[Mets]])
                  jsonToProduct[A, Mets, Mels](p, resArr, obj)
                case arr: JsonArray =>
                  throw new JsonCodecException("array value not supported")
                case _ =>
                  throw new JsonCodecException("raw value not supported")

        case s: Mirror.SumOf[A] =>
          throw new JsonCodecException("ADT not supported")

    private inline def productToJson[A, Mets, Mels](
        p: Product,
        i: Int = 0,
        jsonObj: JsonObject
    ): JsonObject =

      val fields = getJsonFields[A]

      inline (erasedValue[Mets], erasedValue[Mels]) match
        // base case
        case _: (EmptyTuple, EmptyTuple) => jsonObj

        case _: (ImplicitlyAny *: metsTail, mel *: melsTail) =>
          println("ImplicitlyAny")

          val fieldName = constValue[mel & String]

          fields.findJsonName(fieldName).foreach { k =>

            val elem = p.productElement(i)
            println(s"elm = ${elem.getClass.getName}")
            val value = fields.toNative(fieldName, elem).orNull
            jsonObj.setByName(k, value)
          }

          productToJson[A, metsTail, melsTail](p, i + 1, jsonObj)

        case _: (met *: metsTail, mel *: melsTail) =>
          println("others")

          val fieldName = constValue[mel & String]
          // val nc = summonInline[NativeConverter[met]]
          val nativeElem = p.productElement(i).asInstanceOf[met]
          fields.findJsonName(fieldName).foreach { k =>
            val value = fields.toNative(fieldName, nativeElem).orNull
            jsonObj.setByName(k, value)
          }

          productToJson[A, metsTail, melsTail](p, i + 1, jsonObj)

    private inline def jsonToProduct[A, Mets, Mels](
        mirror: Mirror.ProductOf[A],
        resArr: Array[Any],
        jsonObj: base.JsonObject,
        i: Int = 0
    ): A =

      val fields = getJsonFields[A]
      val defaults = getFieldsDefaults[A]

      inline (erasedValue[Mets], erasedValue[Mels]) match
        case _: (EmptyTuple, EmptyTuple) =>
          mirror.fromProduct(ArrayProduct(resArr))

        case _: (met *: metsTail, mel *: melsTail) =>
          // val nc = summonInline[NativeConverter[met]]
          val nc = summonInline[JsonValueConverter[met]]
          val key = constValue[mel & String]

          val jsonFieldName = fields.findJsonName(key)

          val value =
            if jsonFieldName.isEmpty
            then defaults.find(_._1 == key).get // get default field value
            else
              val jname = jsonFieldName.get
              if nc.isInstanceOf[JsonValueConverter[Date]]
              then
                // if Date, process annotation before
                val jsonDate = jsonObj.getByName(jname).orNull
                val convValue = fields.fromNative(key, jsonDate).orNull
                nc.convert(convValue)
              else
                val jsonValue = nc.convert(jsonObj.getByName(jname).orNull)
                fields.fromNative(key, jsonValue).orNull

          resArr(i) = value

          jsonToProduct[A, metsTail, melsTail](mirror, resArr, jsonObj, i + 1)
