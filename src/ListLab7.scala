import ListLab7.BT
import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global



object ListLab7 {

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


  def main(args: Array[String]): Unit = {
    var tree = createTree(25)

    time{
      val f1 = Future{ MaxInTree(tree)}
      println(Await.result(f1, Duration.Inf))
      Await.result(f1, Duration.Inf).toString
    }
    println("PRZERWA")
    time{
      val f2 = Future{MaxInTreeTime(tree,1) }
      println(Await.result(f2, Duration.Inf))
      Await.result(f2, Duration.Inf).toString
    }
    time{
      val f2 = Future{MaxInTreeTime(tree,2) }
      println(Await.result(f2, Duration.Inf))
      Await.result(f2, Duration.Inf).toString
    }
    time{
      val f2 = Future{MaxInTreeTime(tree,3) }
      println(Await.result(f2, Duration.Inf))
      Await.result(f2, Duration.Inf).toString
    }
    time{
      val f2 = Future{MaxInTreeTime(tree,4) }
      println(Await.result(f2, Duration.Inf))
      Await.result(f2, Duration.Inf).toString
    }
    time{
      val f2 = Future{MaxInTreeTime(tree,5) }
      println(Await.result(f2, Duration.Inf))
      Await.result(f2, Duration.Inf).toString
    }
    println("\n")
    var list = List(3,6,2,6,4,8,4,3,5,9,1)
    var list2 = List(3,6,2,6,4,8,4,3,5,9,1)
    time{
      val f = Future{mergeSort(createList(3000))}
      println(Await.result(f, Duration.Inf))
      Await.result(f, Duration.Inf).toString
    }
    println("PRZERWA")
    time{
      val f = Future{mergeSortTime(createList(3000),2)}
      println(Await.result(f, Duration.Inf))
      Await.result(f, Duration.Inf).toString
    }
    time{
      val f = Future{mergeSortTime(createList(3000),3)}
      println(Await.result(f, Duration.Inf))
      Await.result(f, Duration.Inf).toString
    }


  }

  def createList(n: Int): List[Int] =
    val r = scala.util.Random()
    if n>0 then  r.nextInt(10000)::createList(n-1) else List()


  def time[T](f: => T): Unit = {
    val start = System.nanoTime()
    val ret = f
    val end = System.nanoTime()
    println(s"Time taken: ${(end - start) / 1000 / 1000} ms")
  }

  def createTree(N: Int): BT[Double] =
    val r = scala.util.Random
    def createTreeRec(N: Int): BT[Double] =
      if N>0 then Node(r.nextDouble(), createTreeRec(N-1),createTreeRec(N-1))
      else Empty
    createTreeRec(N)

  def MaxInTree(tree: BT[Double]): Double =
    tree match
      case Empty => 0
      case Node(v,left,right) => val x = MaxInTree(left)
        val y = MaxInTree(right)
        if v > x && v > y then v
        else if x > y then x
        else y

  def MaxInTreeTime(tree: BT[Double],x: Int): Double =
    if x <= 0 then
      val f = Future{ MaxInTree(tree) }
      Await.result(f,Duration.Inf)
    else
      tree match
        case Empty => 0
        case Node(v,left,right) => val k = MaxInTreeTime(left,x-1)
          val y = MaxInTreeTime(right,x-1)
          if v > k && v > y then v
          else if k > y then k
          else y

  def splitList[A](list: List[A]): (List[A],List[A],Int) =
    val length = list.length;
    def splitListRec[A](list: List[A],endList: List[A],counter: Int): (List[A],List[A],Int) =
      if counter > 0 then splitListRec(list.tail,list.head::endList,counter-1) else (list, endList,length/2)
    splitListRec(list,List(),length/2)

  def combineLists(lista: List[Int],listb: List[Int]): List[Int] =
    (lista,listb) match
      case (Nil,l) => l
      case (l,Nil) => l
      case (Nil,Nil) => Nil
      case (h1::t1, h2::t2) => if h1<h2 then h1::combineLists(lista.tail,listb)
                               else if h1==h2 then h1::h2::combineLists(lista.tail,listb.tail)
                               else h2::combineLists(lista,listb.tail)


  def mergeSort(list: List[Int]): List[Int] =
    val x = splitList(list)
    if x._3 > 0 then combineLists(mergeSort(x._1) ,mergeSort(x._2)) else List(list.head)


  def mergeSortTime(list: List[Int], n: Int): List[Int] =
    if n > 0 then
               val x = splitList(list)
               if x._3 > 0 then combineLists(mergeSortTime(x._1,n-1) ,mergeSortTime(x._2,n-1)) else List(list.head)
    else
      val f = Future{ mergeSort(list) }
      Await.result(f,Duration.Inf)


}
