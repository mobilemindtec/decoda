package trash

import scala.compiletime.*
import scala.deriving.*
import scala.quoted.*


trait Eq[T]:
  def eqv(x: T, y: T): Boolean

object Eq:
  given Eq[String] with
    def eqv(x: String, y: String) = x == y

  given Eq[Int] with
    def eqv(x: Int, y: Int) = x == y

  def eqProduct[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = body(x, y)

  def eqSum[T](body: (T, T) => Boolean): Eq[T] =
    new Eq[T]:
      def eqv(x: T, y: T): Boolean = body(x, y)

  def summonInstances[T: Type, Elems: Type](using Quotes): List[Expr[Eq[?]]] =
    Type.of[Elems] match
      case '[elem *: elems] => deriveOrSummon[T, elem] :: summonInstances[T, elems]
      case '[EmptyTuple]    => Nil

  def deriveOrSummon[T: Type, Elem: Type](using Quotes): Expr[Eq[Elem]] =
    Type.of[Elem] match
      case '[T] => deriveRec[T, Elem]
      case _    => '{ summonInline[Eq[Elem]] }

  def deriveRec[T: Type, Elem: Type](using Quotes): Expr[Eq[Elem]] =
    Type.of[T] match
      case '[Elem] => '{ error("infinite recursive derivation") }
      case _       => derivedMacro[Elem] // recursive derivation

  inline def derived[T]: Eq[T] = ${ derivedMacro[T] }

  def derivedMacro[T: Type](using Quotes): Expr[Eq[T]] =

    val ev: Expr[Mirror.Of[T]] = Expr.summon[Mirror.Of[T]].get

    ev match
      case '{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonInstances[T, elementTypes]
        def eqProductBody(x: Expr[Product], y: Expr[Product])(using Quotes): Expr[Boolean] = {
          if elemInstances.isEmpty then
            Expr(true)
          else
            elemInstances.zipWithIndex.map {
              case ('{ $elem: Eq[t] }, index) =>
                val indexExpr = Expr(index)
                val e1 = '{ $x.productElement($indexExpr).asInstanceOf[t] }
                val e2 = '{ $y.productElement($indexExpr).asInstanceOf[t] }
                '{ $elem.eqv($e1, $e2) }
            }.reduce((acc, elem) => '{ $acc && $elem })
          end if
        }
        '{ eqProduct((x: T, y: T) => ${eqProductBody('x.asExprOf[Product], 'y.asExprOf[Product])}) }

      case '{ $m: Mirror.SumOf[T] { type MirroredElemTypes = elementTypes }} =>
        val elemInstances = summonInstances[T, elementTypes]
        val elements = Expr.ofList(elemInstances)

        def eqSumBody(x: Expr[T], y: Expr[T])(using Quotes): Expr[Boolean] =
          val ordx = '{ $m.ordinal($x) }
          val ordy = '{ $m.ordinal($y) }
          '{ $ordx == $ordy && $elements($ordx).asInstanceOf[Eq[Any]].eqv($x, $y) }

        '{ eqSum((x: T, y: T) => ${eqSumBody('x, 'y)}) }
  end derivedMacro
end Eq
