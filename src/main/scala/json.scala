import java.util.Date
import scala.collection.mutable

enum OptOmit:
  case OmitNull, NoOmit

trait DateFormatter:
  def parse(date: String, patter: String): Date
  def format(date: Date, patter: String): String

trait JsonObject:
  def getByName(name: String): Option[Any]
  def setByName(name: String, value: Any): Unit
  def stringify(): String

trait JsonArray:
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


