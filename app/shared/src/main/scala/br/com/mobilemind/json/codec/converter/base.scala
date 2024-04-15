package br.com.mobilemind.json.codec.converter

import java.util.Date
import scala.collection.mutable

enum OptOmit:
  case OmitNull, NoOmit

object base:

  
  trait DateFormatter:
    def parse(date: String, patter: String): Date
    def format(date: Date, patter: String): String
  
  trait Json
  
  case class JsonValue(value: Any) extends Json
  
  trait JsonObject extends Json:
    def getByName(name: String): Option[Any]
    def setByName(name: String, value: Any): Unit
    def stringify(): String
    def parser(json: String): Json
  
  trait JsonArray extends Json:
    def get(i: Int): Any
    def size: Int
    def add(v: Any): Unit
  
    def map[T](f: Any => T): List[T] =
      val buff = mutable.ListBuffer[T]()
      for i <- 0 until size do
        buff.addOne(f(get(i)))
      buff.toList
  
  trait JsonParser:
    def parse(s: String): JsonObject
  
  trait JsonCreator:
    def empty: JsonObject
  

