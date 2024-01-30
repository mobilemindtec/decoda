import json.converter.base.{JsonArray, JsonObject}

import scala.collection.mutable

object infra:

  class Json extends JsonObject:

    val fieldsNames: mutable.ListBuffer[String] = mutable.ListBuffer()
    val fields: mutable.Map[String, Any] = mutable.Map()

    private def addAll(data: Map[String, Any]): Json =
      fields.addAll(data)
      this

    override def getByName(name: String): Option[Any] =
      fields.get(name)

    override def setByName(name: String, value: Any): Unit =
      fieldsNames.addOne(name)
      fields.put(name, value)

    override def stringify(): String =
      val l = fieldsNames.map:
        k =>
          fields.get(k) match
            case Some(j: JsonObject) => s"$k: ${j.stringify()}"
            case Some(l: List[_]) =>
              l.headOption match
                case Some(_: JsonObject) =>
                  s"$k: [${l.asInstanceOf[List[JsonObject]].map(_.stringify()).mkString(", ")}]"
                case _ =>
                  s"$k: [${l.mkString(", ")}]"
            case Some(v) => s"$k: $v"
            case _ => s"$k: ???"

      s"{${l.mkString(", ")}}"

    override def toString: String = fields.toString()

  object Json:
    def apply(values: (String, Any)*): Json = new Json().addAll(values.toMap)

  class JArray extends JsonArray:

    private val items = mutable.ListBuffer[Any]()

    override def add(v: Any): Unit = items.addOne(v)

    override def get(i: Int): Any = items(i)

    override def size: Int = items.size

    def addAll(data: Seq[Any]): JArray =
      items.addAll(data)
      this

    override def toString: String = items.toString()

  object JArray:
    def apply(vs: Any*): JArray = new JArray().addAll(vs)