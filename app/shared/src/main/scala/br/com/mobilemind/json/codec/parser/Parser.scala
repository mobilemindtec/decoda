package br.com.mobilemind.json.codec.parser

import br.com.mobilemind.json.codec.JsonCodecException

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Parser:

  import TokenType._
  import AstValue._

  enum TokenType:
    case LeftBrace
    case RightBrace
    case LeftBracket
    case RightBracket
    case Colon
    case Comma
    case Str(value: String)
    case Num(value: String)
    case True
    case False
    case Null
    case NewLine

  extension (token: TokenType)
    def toString: String = token match
      case LeftBrace    => "{"
      case RightBrace   => "}"
      case LeftBracket  => "["
      case RightBracket => "]"
      case Colon        => ":"
      case Comma        => ","
      case Str(s)       => s"S:$s"
      case Num(s)       => s"N:$s"
      case True         => "B:true"
      case False        => "B:false"
      case Null         => "U:null"
      case NewLine      => "\n"

  case class AstProp(name: String, value: AstValue)

  enum AstValue:
    case StringLiteral(s: String)
    case FloatLiteral(f: Float)
    case DoubleLiteral(f: Double)
    case IntLiteral(i: Int)
    case LongLiteral(i: Long)
    case ShortLiteral(i: Short)
    case BoolLiteral(b: Boolean)
    case NullLiteral
    case ArrayAst(items: Array[AstValue] = Array())
    case ObjectAst(props: Seq[AstProp] = Nil)

  extension (value: AstValue)
    def toString: String =
      value match
        case StringLiteral(v) => v
        case FloatLiteral(v)  => v.toString
        case DoubleLiteral(v) => v.toString
        case IntLiteral(v)    => v.toString
        case LongLiteral(v)   => v.toString
        case ShortLiteral(v)  => v.toString
        case BoolLiteral(v)   => if v then "true" else "false"
        case NullLiteral      => "null"
        case obj: ObjectAst   => "[object]"
        case arr: ArrayAst    => "[array]"

    def toRawValue: Any =
      value match
        case StringLiteral(v: String) => v
        case FloatLiteral(v: Float)   => v
        case DoubleLiteral(v: Double) => v
        case IntLiteral(v: Int)       => v
        case LongLiteral(v: Long)     => v
        case ShortLiteral(v: Short)   => v
        case BoolLiteral(v: Boolean)  => v
        case NullLiteral              => null
        case obj: ObjectAst           => obj.toMap
        case arr: ArrayAst            => arr.toSeqRawValue

  extension (arr: ArrayAst)
    def toSeqRawValue: Seq[Any] =
      arr.items.map {
        case obj: ObjectAst => obj.toMap
        case arr: ArrayAst  => arr.toSeqRawValue
        case v: AstValue    => v.toRawValue
      }

  extension (value: ObjectAst)
    def toMap: Map[String, Any] =
      value.props.foldLeft(Map[String, Any]()) { (map, prop) =>
        val raw =
          prop.value match
            case obj: ObjectAst => obj.toMap
            case arr: ArrayAst  => arr.toSeqRawValue
            case _              => prop.value.toRawValue
        map + (prop.name -> raw)
      }

  private def selectValueType(value: String) =
    value match
      case "true"  => True
      case "false" => False
      case "null"  => Null
      case _       => Num(value)

  @tailrec
  private def findString(
      str: List[Char],
      acc: List[Char] = Nil
  ): (List[Char], List[Char]) =
    str match
      case '"' :: xs => (acc, xs)
      case x :: xs   => findString(xs, acc :+ x)
      case _         => throw new JsonCodecException("premature end of string")

  @tailrec
  private def findValue(
      str: List[Char],
      acc: List[Char] = Nil
  ): (TokenType, List[Char], List[Char]) =
    str match
      case '\n' :: xs => (NewLine, acc, xs)
      case ',' :: xs  => (Comma, acc, xs)
      case '}' :: xs  => (RightBrace, acc, xs)
      case ']' :: xs  => (RightBracket, acc, xs)
      case ' ' :: xs  => findValue(xs, acc)
      case x :: xs    => findValue(xs, acc :+ x)
      case _          => throw new JsonCodecException("premature end of value")

  @tailrec
  private def createTokens(
      str: List[Char],
      acc: List[TokenType] = Nil
  ): List[TokenType] =
    str match
      case (' ' | '\n') :: xs =>
        createTokens(xs, acc)
      case '{' :: xs => createTokens(xs, acc :+ LeftBrace)
      case '}' :: xs => createTokens(xs, acc :+ RightBrace)
      case '[' :: xs => createTokens(xs, acc :+ LeftBracket)
      case ']' :: xs => createTokens(xs, acc :+ RightBracket)
      case ':' :: xs => createTokens(xs, acc :+ Colon)
      case ',' :: xs => createTokens(xs, acc :+ Comma)
      case '"' :: xs =>
        val (value, rest) = findString(xs)
        val strVal = value.mkString
        createTokens(rest, acc :+ Str(strVal))
      case x :: xs =>
        val (next_token_typ, value, rest) = findValue(x :: xs)
        val strVal = value.mkString
        val typ = selectValueType(strVal)
        createTokens(rest, acc :+ typ :+ next_token_typ)
      case Nil => acc

  @tailrec
  private def parseTokenToArray(
      tokens: List[TokenType],
      acc: Array[AstValue] = Array()
  ): (List[TokenType], Array[AstValue]) =
    tokens match
      case RightBracket :: rest => (rest, acc)
      case token :: rest =>
        val (value, rst) = parseTokenToAst(token, rest)
        parseTokenToArray(rst, acc :+ value)
      case Nil => (Nil, acc)

  @tailrec
  private def parseTokenToAst(
      token: TokenType,
      rest: List[TokenType] = Nil
  ): (AstValue, List[TokenType]) =
    token match
      case Str(s) => (StringLiteral(s), rest)
      case Num(s) =>
        if s.contains('.')
        then (DoubleLiteral(s.toDouble), rest)
        else (LongLiteral(s.toInt), rest)
      case True  => (BoolLiteral(true), rest)
      case False => (BoolLiteral(false), rest)
      case Null  => (NullLiteral, rest)
      case LeftBrace =>
        val (props, rst) = parseTokenToProperties(rest, Nil)
        (ObjectAst(props), rst)
      case LeftBracket =>
        val (rst, arr) = parseTokenToArray(rest, Array())
        (ArrayAst(arr), rst)
      case Comma | NewLine => parseTokenToAst(rest.head, rest.tail)
      case _ =>
        throw new JsonCodecException("wrong token type ${token.toString}")

  @tailrec
  private def parseTokenToProperties(
      tokens: List[TokenType],
      props: List[AstProp]
  ): (List[AstProp], List[TokenType]) =
    tokens match
      // field:value::rest
      case Str(fld) :: _ :: v :: xs =>
        val (literal, rst) = parseTokenToAst(v, xs)
        parseTokenToProperties(rst, props :+ AstProp(fld, literal))
      case (Comma | NewLine) :: xs => parseTokenToProperties(xs, props)
      case RightBrace :: rts       => (props, rts)
      case _                       => throw new JsonCodecException("wrong end prop type")

  private def parseTokens(tokens: List[TokenType]) = tokens match
    case x :: xs =>
      val (value, _) = parseTokenToAst(x, xs)
      value
    case Nil => ObjectAst(Nil)

  @tailrec
  def tokenize(str: List[Char]): List[TokenType] =
    str match
      case (' ' | '\n') :: xs => tokenize(xs)
      case '{' :: _           => createTokens(str)
      case '[' :: _           => createTokens(str)
      case '"' :: _           => createTokens(str)
      case _                  => selectValueType(str.mkString) :: Nil

  def parse(str: String, debug: Boolean = false): AstValue =
    val tokens = tokenize(str.toList)
    if debug then debugTokens(tokens)
    val ast = parseTokens(tokens)
    if debug then debugAst(ast)
    ast

  def format(ast: AstValue): String =
    ast match
      case StringLiteral(s: String) => s"\"$s\""
      case DoubleLiteral(f: Double) => f"$f%1.2f"
      case FloatLiteral(f: Float)   => f"$f%1.2f"
      case IntLiteral(i: Int)       => i.toString
      case LongLiteral(i: Long)     => i.toString
      case ShortLiteral(i: Short)   => i.toString
      case BoolLiteral(b: Boolean)  => b.toString
      case NullLiteral              => "null"
      case ArrayAst(l: Array[AstValue]) =>
        val str =
          l.foldLeft(StringBuilder("[")) { (acc, v) =>
            acc.append(format(v)).append(", ")
          }

        if str.endsWith(", ")
        then str.delete(str.length() - 2, str.length())

        str.append("]").toString()

      case ObjectAst(l: Seq[AstProp]) =>
        val str =
          l.foldLeft(StringBuilder("{")) { (acc, v) =>
            acc
              .append("\"")
              .append(v.name)
              .append("\": ")
              .append(format(v.value))
              .append(", ")

          }

        if str.endsWith(", ")
        then str.delete(str.length() - 2, str.length())

        str.append("}").toString()

  private def debugTokens(tokens: List[TokenType]): Unit =
    println("==> tokens")
    printTokens(tokens)
    println("<== tokens")

  private def debugAst(ast: AstValue): Unit =
    println("==> ast")
    printAst(ast)
    println("\n<== ast")

  private def printAst(node: AstValue): Unit =
    node match
      case ArrayAst(values) =>
        print("[")
        values.foreach(printAst)
        print("]")
      case ObjectAst(props) =>
        print("{")
        props.foreach { it =>
          print(s"${it.name}=")
          printAst(it.value)
          print(",")
        }
        print("}")
      case other => print(other.toString)

  @tailrec
  private def printTokens(tokens: List[TokenType]): Unit =
    tokens match
      case x :: xs =>
        print(s"${x.toString}, ")
        printTokens(xs)
      case Nil => print("\n")
