package macros

import scala.quoted._

inline def createInstance[T]: T = ${createInstanceImpl[T]}

def createInstanceImpl[T: Type](using Quotes): Expr[T] = {

  import quotes.*, quotes.reflect.*
  
  val sym = TypeTree.of[T].symbol
  val comp = sym.companionClass
  val mod = Ref(sym.companionModule)
  //val names =
  //  for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
  //  yield p.name
  //val namesExpr: Expr[List[String]] =
  //  Expr.ofList(names.map(Expr(_)))

  val body = comp.tree.asInstanceOf[ClassDef].body
  val idents: List[Ref] =
    for case deff @ DefDef(name, _, _, _) <- body
    if name.startsWith("$lessinit$greater$default")
    yield mod.select(deff.symbol)
  //val identsExpr: Expr[List[Any]] =
 //   Expr.ofList(idents.map(_.asExpr))
  
   Apply(
      Select(
        New(TypeTree.of[T]),
        TypeRepr.of[T].typeSymbol.primaryConstructor
      ),
      idents.map(_.asExpr.asTerm)
    ).asExprOf[T]

    // Usamos `TypeTree` para representar o tipo
  //val newInstance = New(TypeTree.of[T])


  // Convertendo a árvore de termos para uma expressão
  //val resultExpr = Expr.betaReduce(newInstance.asExpr)

  //resultExpr.asExprOf[T]
}
