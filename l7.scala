import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Random

object Main {

    val check_circle = {
        (x: Float, y: Float) => { x*x + y*y < 1 }
    }
    
    def calcPi_parallel(n_points: Integer, n_futures: Integer): Double = {
        val futures = for (p <- 1 to n_futures) yield Future {
            val r = new Random
            var count = 0
            for (i <- 1 to (n_points / n_futures))
                if check_circle(r.nextFloat(), r.nextFloat()) then count = count + 1
            count
        }
        val results = futures.map(Await.result(_, duration.Duration.Inf))
        4.0 * (results.sum) / n_points
    }

    def calcPi(n_points: Integer): Double = {
        val r = new Random
        var count = 0
        for (i <- 1 to n_points)
            if check_circle(r.nextFloat(), r.nextFloat()) then count = count + 1
        4.0 * count / n_points
    }

    def make_list_descending(n: Int): List[Int] = {
        if n <= 0 then List()
        else n::make_list_descending(n - 1)
    }

    def mergesort_parallel(list_to_sort: List[Int], level: Int): List[Int] = {
        def split(list: List[Int]): (List[Int], List[Int]) = {
            list match {
                case List() => (List(), List())
                case h1::h2::t => {
                    split(list.tail.tail) match {
                        case (l, r) => (h1::l, h2::r)
                    }
                }
                case h::t => {
                    split(list.tail) match {
                        case (l, r) => (h::l, r)
                    }
                }
            }
        }
        def merge(list1: List[Int], list2: List[Int]): List[Int] = {
            (list1, list2) match {
                case (List(), list2) => list2
                case (list1, List()) => list1
                case (h1::t1, h2::t2) => {
                    if h1 <= h2
                    then h1::merge(t1, list2)
                    else h2::merge(list1, t2)
                }
            }
        }
        list_to_sort match {
            case List() => List()
            case List(h) => List(h)
            case _ => {
                val (half1, half2) = split(list_to_sort)
                if level <= 0 then merge(mergesort(half1), mergesort(half2))
                else {
                    val list1 = Future{mergesort_parallel(half1, level - 1)}
                    val list2 = Future{mergesort_parallel(half2, level - 1)}
                    val sorted1 = Await.result(list1, duration.Duration.Inf)
                    val sorted2 = Await.result(list2, duration.Duration.Inf)
                    merge(sorted1, sorted2)
                }
            }
        }
    }

    def mergesort(list_to_sort: List[Int]): List[Int] = {
        def split(list: List[Int]): (List[Int], List[Int]) = {
            list match {
                case List() => (List(), List())
                case h1::h2::t => {
                    split(list.tail.tail) match {
                        case (l, r) => (h1::l, h2::r)
                    }
                }
                case h::t => {
                    split(list.tail) match {
                        case (l, r) => (h::l, r)
                    }
                }
            }
        }
        def merge(list1: List[Int], list2: List[Int]): List[Int] = {
            (list1, list2) match {
                case (List(), list2) => list2
                case (list1, List()) => list1
                case (h1::t1, h2::t2) => {
                    if h1 <= h2
                    then h1::merge(t1, list2)
                    else h2::merge(list1, t2)
                }
            }
        }
        list_to_sort match {
            case List() => List()
            case List(h) => List(h)
            case _ => {
                val (half1, half2) = split(list_to_sort)
                merge(mergesort(half1), mergesort(half2))
            }
        }
    }

    def main(args: Array[String]): Unit = {
        println("\n\nCALCULATING PI\n")
        val t0_calcPi_nonparallel = System.nanoTime()
        calcPi(1000000)
        val t1_calcPi_nonparallel = System.nanoTime()
        println("Non-Parallel\n\tElapsed time: " + (t1_calcPi_nonparallel - t0_calcPi_nonparallel) + "ns\n")
        val t0_calcPi_parallel = System.nanoTime()
        calcPi_parallel(1000000, 3)
        val t1_calcPi_parallel = System.nanoTime()
        println("Parallel\n\tElapsed time: " + (t1_calcPi_parallel - t0_calcPi_parallel) + "ns\n")

        println("\n\nMERGESORT\n")
        var l1 = make_list_descending(1000)
        val t0_mergesort_nonparallel = System.nanoTime()
        l1 = mergesort(l1)
        val t1_mergesort_nonparallel = System.nanoTime()
        println("Non-Parallel\n\tElapsed time: " + (t1_mergesort_nonparallel - t0_mergesort_nonparallel) + "ns\n")
        l1 = make_list_descending(1000)
        val t0_mergesort_parallel = System.nanoTime()
        l1 = mergesort_parallel(l1, 1)
        val t1_mergesort_parallel = System.nanoTime()
        println("Parallel\n\tElapsed time: " + (t1_mergesort_parallel - t0_mergesort_parallel) + "ns\n")
    }
}
