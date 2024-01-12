import scala.deriving.*

object Tuples:
  def to[A <: Product](value: A)(using mirror: Mirror.ProductOf[A]
  ): mirror.MirroredElemTypes =
    Tuple.fromProductTyped(value)

  def from[A](value: Product)(
    using mirror: Mirror.ProductOf[A], ev: value.type <:< mirror.MirroredElemTypes
  ): A =
    mirror.fromProduct(value)

  final case class Vehicle(manufacturer: String, wheels: Int)

  def test =
    Tuples.to(Vehicle(manufacturer = "Lada", wheels = 4))
    // ("Lada", 4)
    Tuples.from[Vehicle](("Simson", 2))
    // Vehicle("Simson", 2)