package io.decoda.converter

import java.util.Date
import scala.collection.mutable

enum OptOmit:
  case OmitNull, NoOmit

object base:

  trait DateFormatter:
    def parse(date: String, pattern: String): Date
    def format(date: Date, pattern: String): String

  trait Json:
    def stringify(): String

  case class JsonValue(value: Any) extends Json:
    override def stringify(): String = s"$value"

  trait JsonObject extends Json:
    def getByName(name: String): Option[Any]
    def setByName(name: String, value: Any): Unit
    def keys: Set[String]
    def toMap: Map[String, Any]
    def toTuple: Seq[(String, Any)]

  trait JsonArray extends Json:
    def get(i: Int): Any
    def size: Int
    def add(v: Any): Unit
    def addAll(vs: Iterable[Any]): Unit
    def toSeq: Seq[Any]
    def map[T](f: Any => T): List[T] =
      val buff = mutable.ListBuffer[T]()
      for i <- 0 until size do buff.addOne(f(get(i)))
      buff.toList

  trait JsonParser:
    def parse(s: String): Json

  trait JsonCreator:
    def mkObject: JsonObject
    def mkArray: JsonArray

    def fromIterable(items: Iterable[Any]): JsonArray =
      val arr = mkArray
      arr.addAll(items)
      arr

    def fromMap(data: Map[String, Any]): JsonObject =
      val obj = mkObject
      data.foreach { (k, v) =>
        obj.setByName(k, v)
      }
      obj
