package monad

import monad.Monad.{Bouquet, BouquetImpl, Rose}

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
    if(bloomDays.length < (minBouquets * minFlowerPerBouquet)) return -1
    if(bloomDays.length < 2) return bloomDays(0)

    def combine(index: Int, direction: Int, mapAndBouquets: (mutable.Map[Int, Bouquet], mutable.Set[Bouquet])): Unit = {
      val dir = index + direction
      val bloom = bloomDays(dir)
      val thisDayBouquet = mapAndBouquets._1.getOrElse(index, BouquetImpl(Rose(bloomDays(index))))
      val merge = thisDayBouquet.flatMap(rose => {
        val neighbor = mapAndBouquets._1.getOrElse(dir, BouquetImpl(Rose(bloom)))
        if(thisDayBouquet.size() < minFlowerPerBouquet && neighbor.size() < minFlowerPerBouquet && rose.day >= bloom) {
          BouquetImpl(rose, neighbor)
        } else {
          thisDayBouquet
        }
      })

      mapAndBouquets._1.put(index, merge)

      if(merge.size() > thisDayBouquet.size()) {
        mapAndBouquets._1.put(dir, merge)
      }
      if(merge.size() == minFlowerPerBouquet) {
        mapAndBouquets._2.add(merge)
      }
    }

    import scala.collection.mutable.PriorityQueue
    @tailrec
    def daysToBouquetRec(heap: mutable.PriorityQueue[(Int, Int)], mapAndBouquets: (mutable.Map[Int, Bouquet], mutable.Set[Bouquet])): Int = {
      if(heap.nonEmpty) {
        val dayWithIndex = heap.dequeue()
        val day = dayWithIndex(0)
        val index = dayWithIndex(1)
        if(index == 0) {
          combine(index, 1, mapAndBouquets)
        } else if(index == bloomDays.length - 1) {
          combine(index, -1, mapAndBouquets)
        } else {
          combine(index, 1, mapAndBouquets)
          combine(index, -1, mapAndBouquets)
        }
        if(mapAndBouquets._2.size == minBouquets) {
          day
        } else {
          daysToBouquetRec(heap, mapAndBouquets)
        }
      } else -1
    }
    val heap = new mutable.PriorityQueue[(Int, Int)]()(Ordering.by[(Int, Int), Int](x => x(0)).reverse).addAll(bloomDays.zipWithIndex)
    daysToBouquetRec(heap, (mutable.Map.empty[Int, Bouquet], mutable.Set.empty[Bouquet]))
  }

  assert(4 == daysToBouquet(Array(1, 2, 4, 9, 3, 4, 1), 2, 2))
  assert(-1 == daysToBouquet(Array(1, 10, 3, 10, 2), 3, 2))
  assert(12 == daysToBouquet(Array(7, 7, 7, 12, 7, 7), 2, 3))
}
