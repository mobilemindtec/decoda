package json.converter

import scala.quoted._

// copy from https://github.com/lampepfl/dotty-macro-examples/blob/main/defaultParamsInference/src/macro.scala
object macros:

  inline def createInstance[T]: T = ${createInstanceImpl[T]}

  def createInstanceImpl[T: Type](using Quotes): Expr[T] = {

    import quotes.*, quotes.reflect.*

    val sym = TypeTree.of[T].symbol
    val comp = sym.companionClass
    val mod = Ref(sym.companionModule)
    val body = comp.tree.asInstanceOf[ClassDef].body
    val idents: List[Ref] =
      for case deff @ DefDef(name, _, _, _) <- body
      if name.startsWith("$lessinit$greater$default")
      yield mod.select(deff.symbol)

     Apply(
        Select(
          New(TypeTree.of[T]),
          TypeRepr.of[T].typeSymbol.primaryConstructor
        ),
        idents.map(_.asExpr.asTerm)
      ).asExprOf[T]
  }
