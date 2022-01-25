package monad

object Monad extends App {

  object Rose {
    def apply(day: Int) = new Rose(day)
  }

  class Rose(val day: Int) {
    override def toString: String = {
      s"$day"
    }

    override def equals(obj: Any): Boolean = {
      obj.asInstanceOf[Rose].day == this.day
    }
  }

  abstract class Bouquet {
    def flatMap(f: Rose => Bouquet): Bouquet

    def size(): Int
  }

  object BouquetImpl {
    def apply(x: Rose): Bouquet = new BouquetImpl(x, EmptyBouquet)

    def apply(x: Rose, other: Bouquet): Bouquet = new BouquetImpl(x, other)
  }

  object EmptyBouquet extends Bouquet {
    def flatMap(f: Rose => Bouquet): Bouquet = this

    def apply(rose: Rose): Bouquet = BouquetImpl(rose)

    def size(): Int = 0
  }

  class BouquetImpl(val rose: Rose, val other: Bouquet = EmptyBouquet) extends Bouquet {
    def flatMap(f: Rose => Bouquet): Bouquet = {
      f(rose)
    }

    // Correctly .size() should be O(1), but for the purposes of this example O(n) is acceptable.
    def size(): Int = {
      1 + other.size()
    }

    override def toString: String = {
      s"rose $rose bouquet $other"
    }

    override def hashCode(): Int = super.hashCode()

    override def equals(obj: Any): Boolean = {
      obj.isInstanceOf[BouquetImpl] && {
        val asBouquet = obj.asInstanceOf[BouquetImpl]
        asBouquet.rose.equals(rose) && asBouquet.other.equals(other)
      }
    }

  }


  println("left identity")
  val rose = Rose(1)
  val roseToBouquet = (x: Rose) => BouquetImpl(x)
  println(BouquetImpl(rose).flatMap(roseToBouquet))
  println(roseToBouquet.apply(rose))
  assert(BouquetImpl(rose).flatMap(roseToBouquet) == (roseToBouquet.apply(rose)))

  println("right identity")
  val bouquet = BouquetImpl(rose)
  println(bouquet.flatMap((rose: Rose) => BouquetImpl(rose)).equals(bouquet))
  assert(bouquet.flatMap((rose: Rose) => BouquetImpl(rose)) == (bouquet))

  println("associativity")
  val rose2 = Rose(2)
  val rose3 = Rose(3)
  val f: Rose => Bouquet = rose => BouquetImpl(rose, BouquetImpl(Rose(rose.day * 2)))
  val g: Rose => Bouquet = rose => BouquetImpl(rose, BouquetImpl(Rose(rose.day + 6)))

  println("--empty case")
  println(EmptyBouquet.flatMap(f).flatMap(g))
  println(EmptyBouquet.flatMap(rose => f(rose).flatMap(g)))

  println("--nonempty case")
  val rose7 = Rose(7)
  val bouquet7 = BouquetImpl(rose7)
  println(bouquet7.flatMap(f).flatMap(g))
  println(bouquet7.flatMap(rose => f(rose).flatMap(g)))
  assert(bouquet7.flatMap(f).flatMap(g) == bouquet7.flatMap(rose => f(rose).flatMap(g)))
  assert(bouquet7.flatMap(f).flatMap(g) == f(rose7).flatMap(g))
}
