package monad

import monad.Monad.{Bouquet, BouquetImpl, EmptyBouquet, Rose}

import scala.annotation.tailrec
import scala.collection.mutable

object RoseGardenMonad extends App {

  /**
   * https://aonecode.com/google-online-assessment#rose-garden
   *
   * Rose Garden
   *
   * Given an array a of roses. For example [1, 2, 4, 9, 3, 4, 1]
   * a[i] means rose i will bloom on day a[i].
   * Here the first rose will bloom on day 1, the second rose will bloom on day 2, etc. etc.
   * Also given an int k, which is the minimum number of adjacent bloom roses required for a bouquet.
   * and an int n, which is the number of bouquets we need.
   * return the earliest day that we can get n bouquets of roses.
   *
   * for example:
   *
   * a = [1, 2, 4, 9, 3, 4, 1], k = 2, n = 2
   * day 1:
   *
   * the first and the last rose bloom.
   * [b, n, n, n, n, n, b]
   *
   * day 2:
   * the second rose bloom
   * [b, b, n, n, n, n, b]
   * Here the first two bloom roses make a bouquet.
   *
   * day 3: [b, b, n, n, b, n, b]
   * day 4: [b, b, n, n, b, b, b]
   * Here the last three bloom roses make a bouquet, meeting the required n = 2 bouquets of bloom roses.
   *
   * So return day 4.
   */
  def daysToBouquet(bloomDays: Array[Int], minFlowerPerBouquet: Int, minBouquets: Int): Int = {
    import scala.collection.mutable.PriorityQueue
    if(bloomDays.length < (minBouquets * minFlowerPerBouquet)) return -1
    if(bloomDays.length < 2) return bloomDays(0)

    def wrapBouquet(index: Int, direction: Int, monadMap: Map[Int, Bouquet]): Map[Int, Bouquet] = {
      val neighborIndex = index + direction
      val neighborBloomDay = bloomDays(neighborIndex)
      val thisDayBouquet = monadMap.getOrElse(index, BouquetImpl(Rose(bloomDays(index))))
      val merge = thisDayBouquet.flatMap(rose => {
        val neighborBouquet = monadMap.getOrElse(neighborIndex, BouquetImpl(Rose(neighborBloomDay)))
        if(thisDayBouquet.size() < minFlowerPerBouquet && neighborBouquet.size() < minFlowerPerBouquet && rose.day >= neighborBloomDay) {
          BouquetImpl(rose, neighborBouquet)
        } else {
          thisDayBouquet
        }
      })

      monadMap + (index -> merge) + (if merge.size() > thisDayBouquet.size() then neighborIndex -> merge else -1 -> EmptyBouquet)
    }

    // Because .equals() of the these Bouquet monads would return true for all bouquets with the same rose(es)
    // we cannot move the Bouquets into a set and then take the size of the set. Instead, we have to count the number
    // of distinct Bouquet references that we have. For that, we have to count distinct hashcodes.
    val countBouquets: Iterable[Bouquet] => Int = _.filter(_.size() >= minFlowerPerBouquet).groupBy(_.hashCode()).keySet.size

    @tailrec
    def daysToBouquetRec(heap: mutable.PriorityQueue[(Int, Int)], monadMap: Map[Int, Bouquet]): Int = {
      if(heap.nonEmpty) {
        val dayWithIndex = heap.dequeue()
        val day = dayWithIndex(0)
        val index = dayWithIndex(1)
        val updatedMonadMap = if(index == 0) {
          wrapBouquet(index, 1, monadMap)
        } else if(index == bloomDays.length - 1) {
          wrapBouquet(index, -1, monadMap)
        } else {
          wrapBouquet(index, -1, wrapBouquet(index, 1, monadMap))
        }

        if(countBouquets(updatedMonadMap.values) >= minBouquets) {
          day
        } else {
          daysToBouquetRec(heap, updatedMonadMap)
        }
      } else -1
    }
    val heap = new mutable.PriorityQueue[(Int, Int)]()(Ordering.by[(Int, Int), Int](x => x(0)).reverse).addAll(bloomDays.zipWithIndex)
    daysToBouquetRec(heap, Map.empty[Int, Bouquet])
  }

  println(daysToBouquet(Array(1, 2, 4, 9, 3, 4, 1), 2, 2))
  println(daysToBouquet(Array(1, 10, 3, 10, 2), 3, 2))
  println(daysToBouquet(Array(7, 7, 7, 12, 7, 7), 2, 3))
  assert(4 == daysToBouquet(Array(1, 2, 4, 9, 3, 4, 1), 2, 2))
  assert(-1 == daysToBouquet(Array(1, 10, 3, 10, 2), 3, 2))
  assert(12 == daysToBouquet(Array(7, 7, 7, 12, 7, 7), 2, 3))
}
