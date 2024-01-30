package trash

trait Random[A]:
    def generate(): A


object Random:

    import scala.compiletime.{erasedValue, summonInline}
    import scala.deriving.*

    inline def summonAll[A <: Tuple]: List[Random[_]] =
      inline erasedValue[A] match
        case _: EmptyTuple => Nil
        case _: (t *: ts) => summonInline[Random[t]] :: summonAll[ts]

    def toTuple(xs: List[_], acc: Tuple): Tuple =
      xs match
        case Nil => acc
        case (h :: t) => h *: toTuple(t, acc)

    inline given derived[A](using m: Mirror.Of[A]): Random[A] =
      lazy val instances = summonAll[m.MirroredElemTypes]
      inline m match
        case s: Mirror.SumOf[A]     => deriveSum(s, instances)
        case p: Mirror.ProductOf[A] => deriveProduct(p, instances)

    def deriveSum[A](s: Mirror.SumOf[A], instances: => List[Random[_]]): Random[A] =
      new Random[A]:
        def generate(): A =
          instances(util.Random.nextInt(instances.size))
            .asInstanceOf[Random[A]]
            .generate()

    def deriveProduct[A](p: Mirror.ProductOf[A], instances: => List[Random[_]]): Random[A] =
      new Random[A]:
        def generate(): A =
          p.fromProduct(
            toTuple(instances.map(_.generate()), EmptyTuple)
          )


object data_test:

  import Random.*

  enum SiteMember:
    case RegisteredUser(id: Long, email: String, isAdmin: Boolean)
    case AnonymousUser(session: String)

  given randStr: Random[String] with
    def generate(): String = util.Random.alphanumeric.take(5).mkString
  
  given randLong: Random[Long] with
    def generate(): Long = util.Random.nextLong(1000000)
  
  given randBool: Random[Boolean] with
    def generate(): Boolean = util.Random.nextBoolean()

  def test =
    println(summon[Random[SiteMember]].generate())
    println(summon[Random[SiteMember]].generate())
    println(summon[Random[SiteMember]].generate())