package packMain

import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.StdIn.readLine

object Main {

  // przykład 1 - sumowanie drzewa
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  val rand = scala.util.Random
  def generateTree(N:Int): Tree[Double] =
    if N == 1 then Leaf(rand.nextDouble())
    else Node(rand.nextDouble(), generateTree(N-1), generateTree(N-1))

  def SumTree(T:Tree[Double]): Double =
    T match
      case l:Leaf[Double] => l.value
      case n:Node[Double] =>
        Thread.sleep(50)
        n.value + SumTree(n.left) + SumTree(n.right)

  def FutSumTree(T:Tree[Double]): Double =
    T match
      case l:Leaf[Double] => l.value
      case n:Node[Double] =>
        val f1 = Future{SumTree(n.left)}
        val f2 = Future{SumTree(n.right)}
        Thread.sleep(50)
        n.value + Await.result(f1, Duration.Inf) + Await.result(f2, Duration.Inf)

  //przykład 2 - praca nad inputem w czasie oczekiwania na kolejny
  def StrToAscii(s:String): String =
    def part(h:Array[Char]): String =
      Thread.sleep(100)
      if h.isEmpty then "" else h.head.toInt + part(h.tail)
    part(s.toCharArray)


  def main(args: Array[String]): Unit = {

    print("Przykład 1: sumowanie drzewa.\nCzas bez użycia programowania współbieżnego:  ")
    val kr = generateTree(5)
    val tr = generateTree(10)
    val sumTreePresentTime = System.nanoTime()
    val x = SumTree(tr)
    print((System.nanoTime()-sumTreePresentTime)+"  wynik sumy: " + x + "\nCzas przy użyciu programowania współbieżnego: ")
    val sumTreeFutureTime = System.nanoTime()
    val x2 = FutSumTree(tr)
    print((System.nanoTime()-sumTreeFutureTime)+"  wynik sumy: " + x2 + "\n\n")

    print("Przykład 2: Praca nad inputem 1 w czasie oczekiwania na input 2. Postaraj się aby wpisanie Stringów zajęło podobną ilość czasu.\nPodaj string 1: ")
    val s1 = readLine()
    val nTime = System.nanoTime()
    val wyn1 = Future{StrToAscii(s1)}
    print("Podaj string 2: ")
    val s2 = readLine()
    val wyn2 = Future{StrToAscii(s2)}
    print("Czas trwania operacji: "+(System.nanoTime()-nTime)+"\nstring 1 w ascii - " + Await.result(wyn1, Duration.Inf) + ", a string 2 w ascii - " + Await.result(wyn2, Duration.Inf) + "\n")
    print("Podaj stringi jeszcze raz, pamiętaj aby pisać tak samo długo.\nPodaj string 1: ")
    val s3 = readLine()
    val nTime2 = System.nanoTime()
    val wyn3 = StrToAscii(s3)
    print("Podaj string 2: ")
    val s4 = readLine()
    val wyn4 = StrToAscii(s4)
    print("Czas trwania operacji: " + (System.nanoTime() - nTime2) + "\nstring 1 w ascii - " + wyn3 + ", a string 2 w ascii - " + wyn4 + "\n")


  }


}
