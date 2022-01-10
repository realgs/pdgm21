import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.sqrt
import scala.util.Random

object l7 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


  def main(args: Array[String]): Unit = {
    val list = List(6,5,4,3,2,1)
    val list2 = List(5,6,2020,6,7,8,4)
    val r = new Random()

    val big = List.fill(100)(r.nextInt(10))
    println("MERGE SORT")
    println("Basic")
    time(mergeSort((a:Int, b:Int) => a < b, big))
    println("Parallel")
    time(mergeSortPar((a:Int, b:Int) => a < b, big))

    println("Prime number")
    println("Basic")
    val list3 = List(2,3,5,7,11,10)
    //println(searchForPrimes(list3))
    time(searchForPrimes(list3))
    println("Parallel")
    time(searchForPrimesPara(list3))

    println("Sum of binary Tree")
    println("Basic")
    val tree = createBinaryTree(3)
    //println(sumOfTreeNodes(tree))
    time(sumOfTreeNodes(tree))
    println("Parallel")
    //println(sumOfTreeNodesPara(tree))
    time(sumOfTreeNodesPara(tree))


  }



  def time[T](f: => T): Unit = {
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    println("Time: " + (end - start) + " in nano")
  }


  //first example - Merge Sort

  def parallel[A,B](task1: => A, task2: => B): (A,B) = {
    val futureTask: Future[B] = Future { task2 }
    val firstTask = task1
    val secondTask = Await.result(futureTask, Duration.Inf)
    (firstTask, secondTask)
  }

  def mergeSort[T](order:(T,T) => Boolean, list: List[T]): List[T] = {
    val partitiion = list.length / 2
    if (partitiion == 0) then list
    else
      val (left, rigt) = list.splitAt(partitiion)
      merge(order,mergeSort(order,left), mergeSort(order,rigt))
  }

  def merge[T](order:(T,T) => Boolean, list1: List[T], list2: List[T]): List[T] = {
    (list1, list2) match
      case (Nil, list2) => list2
      case (list1, Nil) => list1
      case (h1::t1, h2::t2) =>
        if (order(h1,h2)) then h1::merge(order, t1, list2)
        else h2::merge(order,list1,t2)
  }

  def mergeSortPar[T](order:(T,T) => Boolean, list:List[T]): List[T] = {
    //czy zostawic ten warunek?
    if list.length < 1000 then mergeSort(order, list)
    else {
      val partittion = list.length / 2
      if partittion == 0 then list
      else
        val (l1, l2) = list.splitAt(partittion)
        val (left, right) = parallel(mergeSort(order, l1), mergeSort(order, l2))
        merge(order, left, right)
    }
  }

  //second Example - find prime numbers in the given List

  def searchForPrimes(list:List[Int]): List[Int] = {
    def searchForPrimesHelper(listHelper:List[Int], result:List[Int]): List[Int] = {
      listHelper match {
        case Nil => result
        case h::t =>
          if isPrime(h) then searchForPrimesHelper(t, result:::List(h))
          else searchForPrimesHelper(t,result)
      }
    }
    searchForPrimesHelper(list,Nil)
  }

  def searchForPrimesPara(list:List[Int]): List[Int] = {
    def primeHelper(listHelper:List[Int], startIndex:Int, endIndex:Int): List[Int] = {
      if (endIndex - startIndex < 100) then searchForPrimes(listHelper)
      else
        val mid = (startIndex + endIndex) / 2;
        val result = for {
          r1 <- Future{primeHelper(listHelper, startIndex, mid)}
          r2 <- Future{primeHelper(listHelper, mid+1, endIndex)}
        } yield r1 ::: r2
        Await.result(result, Duration.Inf)
    }
    primeHelper(list, 0, list.length-1)
  }


  def isPrime(n:Int): Boolean = {
    if n < 2 then false
    else
      var check = true
      for (i <- 2 to n-1) {
        if (n % i == 0) then check = false
      }
      check
  }





  //third example - Sum of elements in binary Tree

  //creating Tree
  def createBinaryTree(depth:Int):BT[Int] = {
    depth match
      case 0 => Empty
      case _ => Node(Random.nextInt(10), createBinaryTree(depth - 1), createBinaryTree(depth - 1))
  }

  //Basic sum of elements

  def sumOfTreeNodes(tree:BT[Int]): Int = {
    def sumOfTreeNodesHelper(toVisit: BT[Int], result:Int): Int = {
      toVisit match
        case Empty => result
        case Node(value, left, right) => sumOfTreeNodesHelper(left, sumOfTreeNodesHelper(right, value + result))
    }
    sumOfTreeNodesHelper(tree,0)
  }


  //Parallel sum of elements

  val maxDepth = 20
  def sumOfTreeNodesPara(tree:BT[Int]): Int = {
    def sumOfTreeNodesParaHelper(toVisit: BT[Int], currDepth:Int): Int = {
      if maxDepth >= currDepth then sumOfTreeNodes(toVisit)
      else {
        def sumHelper(visit: BT[Int], depth: Int, result: Int): Int = {
          visit match {
            case Empty => result
            case Node(value, left, right) =>
              val res = for {
                r1 <- Future {
                  sumHelper(left, depth - 1, result + value)
                }
                r2 <- Future {
                  sumHelper(right, depth - 1, result + value)
                }
              } yield (r1 + r2)
              Await.result(res, Duration.Inf)
          }
        }

        sumHelper(toVisit, currDepth, 0)
      }
    }
    sumOfTreeNodesParaHelper(tree, maxDepth)
  }



}
