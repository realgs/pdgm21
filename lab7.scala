import java.lang.System.*
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

object lab7 {

  def parallel[A,B](task1: => A , task2: => B):(A,B) = {
    val future: Future[B] = Future{task2}
    val left: A = task1
    val right:B = Await.result(future, Duration.Inf)
    (left,right)
  }

sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](element: A, left: BT[A], right: BT[A]) extends BT[A]

def treeSum(tree: BT[Int], depth: Int):Int = {
  def treeSumIn(VisitList: BT[Int]): Int =
    VisitList match
      case Empty => 0
      case Node(v, l, r) => v + treeSumIn(l) + treeSumIn(r)
  treeSumIn(tree)
}

def treeSumPar(tree: BT[Int],depth: Int): Int = {
  def treeSumParIn(VisitList: BT[Int], actdepth: Int): Int =
    if actdepth <= depth then treeSum(VisitList,actdepth)
    else
    VisitList match
      case Empty => 0
      case Node(v, l, r) =>
        val (left, right) = parallel(treeSumParIn(l, depth - 1), treeSumParIn(r, depth - 1))
        v + left + right

  treeSumParIn(tree, depth)
}

def TreeMax(tree: BT[Int]): Int = {
  tree match{
    case Empty => 0
    case Node(v, l, r) =>
      val possibleMax = TreeMax(l)
      val possibleMax2 = TreeMax(r)
      if v > possibleMax && v > possibleMax2 then v
      else if possibleMax > possibleMax2 then possibleMax
      else possibleMax2
  }
}

def TreeMaxPar(tree: BT[Int] , depth: Int): Int = {
  def TreeMAxParIn(tree: BT[Int] , actdepth: Int): Int =
    if actdepth <= depth then TreeMax(tree)
    else
      tree match {
        case Empty => 0
        case Node(v,l,r) =>
          val possibleMax = parallel(TreeMAxParIn(l,depth-1) , TreeMAxParIn(r,depth-1))
          if v > possibleMax._1 && v > possibleMax._2 then v
          else if possibleMax._1 > possibleMax._2 then possibleMax._1
          else possibleMax._2
      }
  TreeMAxParIn(tree,depth)
}

  def sumList(tab: Array[Int]): Int ={
    var sum = 0
    for (i <- 0 to tab.length - 1) {
      sum += tab(i)
    }
    return sum
  }

  def sumListFut(tab: Array[Int]): Int = {
    def sumListFutIn(tab: Array[Int], start: Int, end: Int): Int = {
      var sum = 0
      for (i <- start to end - 1) {
        sum += tab(i)
      }
      return sum
    }

    val f1 = Future{sumListFutIn(tab, 0, tab.length / 6)}
    val f2 = Future{sumListFutIn(tab, tab.length / 6, tab.length / 3)}
    val f3 = Future{sumListFutIn(tab, tab.length / 3, tab.length / 2)}
    val f4 = Future{sumListFutIn(tab, tab.length / 2, tab.length * 2 / 3)}
    val f5 = Future{sumListFutIn(tab, tab.length * 2 / 3, tab.length * 5 / 6)}
    val f6 = Future{sumListFutIn(tab, tab.length * 5 / 6, tab.length)}

    return Await.result(f1,Duration.Inf) + Await.result(f2,Duration.Inf) + Await.result(f3,Duration.Inf)+Await.result(f4,Duration.Inf)+Await.result(f5,Duration.Inf)+Await.result(f6,Duration.Inf)
  }

  def generateArray(size: Int, range:Int ): Array[Int] = {
    val arr = new Array[Int](size)
    val rnd = new Random()
    for(i <- 0 to size-1){
      arr(i) =  rnd.nextInt(range)
    }
    return arr
  }
  
  def TreeGenerator(depth: Int): BT[Int] = {
    val random = scala.util.Random
    depth match {
      case 0 => Empty
      case _ => Node(random.nextInt(100), TreeGenerator(depth - 1), TreeGenerator(depth - 1))
    }
  }

    def main(args: Array[String]): Unit = {
      val b = generateArray(100000,100)
      val a = TreeGenerator(20)
      var time = nanoTime()
      println(treeSum(a,20))
      println("tree Sum no parell: " + (nanoTime()-time))
      var time2 = nanoTime()
      println(treeSumPar(a,20))
      println("tree sum parell: " + (nanoTime()-time2))
      var time3 = nanoTime()
      println(TreeMax(a))
      println("tree max no parell: " + (nanoTime()-time3))
      var time4 = nanoTime()
      println(TreeMaxPar(a,20))
      println("tree max parell: " + (nanoTime()-time4))
      var time5 = nanoTime()
      println(sumList(b))
      println("sum array noparell: " + (nanoTime()-time5))
      var time6 = nanoTime()
      println(sumListFut(b))
      println("sum array parell: " + (nanoTime()-time6))
    }
}
// wyniki są podawane w ns
// dla Drzewa o głębokości 5:
  //tree Sum no parell: 2570600
  //tree sum parell: 29500
  //tree max no parell: 24500
  //tree max parell: 23100
//dla Drzewa o głębokości 10:
  //tree Sum no parell: 2858700
  //tree sum parell: 83300
  //tree max no parell: 248000
  //tree max parell: 403700
//dla Drzewa o głębokości 20:
  // tree Sum no parell: 10357500
  // tree sum parell: 5818200
  // tree max no parell: 11888400
  // tree max parell: 8771100
//Dla tablicy o długości 10
  //sum array noparell: 1934000
  //sum array parell: 91496200
//Dla tablicy o długości 1000
  //sum array noparell: 2193300
  //sum array parell: 90557200
//Dla tablicy o długości 100000
  //sum array noparell: 7541300
  //sum array parell: 115427000
//Dla tabicy o długości 10000000
  //sum array noparell: 23639000
  //sum array parell: 121661500

