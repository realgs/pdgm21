import sun.security.util.Length

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.math.random
import scala.util.Random

object L7 {

  def printTimeMilis[A](code: => A): Unit ={
    val time0 = System.nanoTime()
    code
    val time1 = System.nanoTime()
    val elapsed = (time1-time0)/1000/1000

    println(s"Time: $elapsed ms")

  }




  def swap(tab: Array[Int])(i: Int)(j: Int): Unit =
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  def choose_pivot(tab: Array[Int])(m:Int)(n:Int): Int =
    tab((m+n)/2)

  def partition(tab: Array[Int])(l:Int)(r:Int): (Int,Int) =
    var i = l
    var j = r
    val pivot = choose_pivot(tab)(l)(r)
    while (i <= j)
      while (tab(i) < pivot) i += 1
      while (pivot < tab(j)) j -= 1
      if (i <= j) then
        swap(tab)(i)(j)
        i += 1
        j -= 1
      else ()
    (i,j)

  def quick(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i,j) = partition(tab)(l)(r)
      if j-l < r-i then
        quick(tab)(l)(j)
        quick(tab)(i)(r)
      else
        quick(tab)(i)(r)
        quick(tab)(l)(j)

  def quickFuture(tab: Array[Int])(l: Int)(r: Int): Unit =
    if l < r then
      val (i,j) = partition(tab)(l)(r)
      if j-l < r-i then
        val f1 = Future(quick(tab)(l)(j))
        val f2 = Future(quick(tab)(i)(r))
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)
      else
        val f1 = Future(quick(tab)(i)(r))
        val f2 = Future(quick(tab)(l)(j))
        Await.result(f1, Duration.Inf)
        Await.result(f2, Duration.Inf)


  def quicksort(tab: Array[Int]): Unit =
    quick(tab)(0)(tab.length-1)

  def quicksortFuture(tab: Array[Int]): Unit =
    quickFuture(tab)(0)(tab.length-1)

  def testQuickSort(): Unit ={
    var length = 1000
    val multiplier = 5

    val arr_10k = Array.fill(10000)(Random.nextInt(1000000))
    val arr_100k = Array.fill(100000)(Random.nextInt(1000000))
    val arr_1m = Array.fill(1000000)(Random.nextInt(1000000))
    val arr_10m = Array.fill(10000000)(Random.nextInt(1000000))

    println("10k elements normal")
    printTimeMilis(quicksort(arr_10k.clone()))
    println("10k elements future")
    printTimeMilis(quicksortFuture(arr_10k.clone()))

    println()

    println("100k elements normal")
    printTimeMilis(quicksort(arr_100k.clone()))
    println("100k elements future")
    printTimeMilis(quicksortFuture(arr_100k.clone()))


    println()

    println("1m elements normal")
    printTimeMilis(quicksort(arr_1m.clone()))
    println("1m elements future")
    printTimeMilis(quicksortFuture(arr_1m.clone()))


    println()

    println("10m elements normal")
    printTimeMilis(quicksort(arr_10m.clone()))
    println("10m elements future")
    printTimeMilis(quicksortFuture(arr_10m.clone()))


  }











  //to jest w sumie troche bez sensu do zrównoleglania bo teorytycznie wyjdzie gorzej bo musimy podzielić liste na pol
  // czyli trzeba w preprocessingu przejsc do polowy co zajmie o(n/2) czasu i potem znowu ją przejsc co znowu zajmie o(n/2)
  // by poszukać elementu wiec zamiast oczekiwanego o(n/2) czasu mamy i tak o(n) wiec nic nie zyskalismy a trzeba jeszcze znać
  // length wiec to kolejne o(n) czyli w teorii zwykle podejscie zajmie o(n) a zrownoleglone o(2n) jesl dobrze to rozumiem

  def splitAt[A](list: List[A],n:Int):(List[A],List[A])={
    def helper[A](list:List[A],n:Int,iter:Int,res:List[A]):(List[A],List[A]) ={
      if iter==n then (list,res) else
        helper(list.tail,n, iter+1, list.head::res)
    }
    helper(list,n,0,Nil)
  }

  def generateList(lenght:Int,max:Int):List[Int] = {
    @tailrec
    def helper(lenght: Int,max:Int,res:List[Int]):List[Int] = {
      lenght match {
        case 0=> res
        case _ => helper(lenght-1,max,Random.nextInt(max)::res)
      }
    }
    helper(lenght,max,Nil)
  }

  def countRepeatsList[A](list:List[A], elem:A):Int ={
    @tailrec
    def helper[A](list:List[A],elem:A,res:Int):Int= {
      list match {
        case head::tail => if elem == head then helper(tail,elem, res+1)
        else helper(tail,elem,res)
        case Nil => res
      }
    }
    helper(list,elem,0)
  }



  // trochę dziwne bo albo nie rozumiem tego albo metoda splitAt jest słabo zoptymalizowana bo oczekiwałem
  // wyników 2-3 razy gorszych a otrzymałem wyniki ~20 razy gorsze

  def countRepeatsFutureList[A](list:List[A], elem:A): Int ={
    val lists = list.splitAt(list.length/2)
    val countReps1 = Future(countRepeatsList(lists._1,elem))
    val countReps2 = Future(countRepeatsList(lists._2,elem))
    Await.result(countReps1,Duration.Inf)+Await.result(countReps2,Duration.Inf)
  }

  // tutaj z moja funkcja splitat wyniki dalej sa 10 razy gorsze niz sekwencyjnie, nie do konca wiem czemu
  def countRepeatsFuture2List[A](list:List[A], elem:A): Int ={
    val lists = splitAt(list,list.length/2)
    val countReps1 = Future(countRepeatsList(lists._1,elem))
    val countReps2 = Future(countRepeatsList(lists._2,elem))
    Await.result(countReps1,Duration.Inf)+Await.result(countReps2,Duration.Inf)
  }





  // to samo tylko z uzyciem tablicy zamiast listy , random access powinien pomóc wiec jednak ma to troche sensu

  def createArray(size:Int,max:Int):Array[Int]={
    var res  = new Array[Int](size)
    for(i<- 0 to size-1)
      res(i) = Random.nextInt(max)

    res
  }



  def countRepeatsArray[T](arr:Array[T], start:Int, end:Int, elem:T):Int = {
    var count=0
    for( i<- start to end-1)
      if(arr(i)==elem) then count+=1

    count
  }


  def countRepeatsFutureArray[T](array: Array[T], elem:T): Int ={
    val len = array.length
    val count1 = Future(countRepeatsArray(array,0,len/2,elem))
    val count2 = Future(countRepeatsArray(array,len/2+1,len,elem))
    Await.result(count1,Duration.Inf)+ Await.result(count2,Duration.Inf)
  }





  def fib(n : Int): BigInt =
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }

  def fibFuture(n: Int): BigInt = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ =>
        val fib1 = Future(fib(n - 1))
        val fib2 = Future(fib(n - 2))
        Await.result(fib1, Duration.Inf) + Await.result(fib2, Duration.Inf)
    }
  }

  def fibiter(n:Int):BigInt = {
    var a=0
    var b =1
    for(i<-1 to n-1)
      b+=a
      a = b-a

    b
  }



  def printTestRepResults(length:Int): Unit ={
    val list =generateList(length,20000)
    val array = createArray(length,20000)


    println(s"countRepeatsList dla $length")
    printTimeMilis(countRepeatsList(list,list.head))


    println(s"countRepeatsFutureList dla $length")
    printTimeMilis(countRepeatsFutureList(list,list.head))

    println(s"countRepeatsFuture2List dla $length")
    printTimeMilis(countRepeatsFuture2List(list,list.head))

    println(s"countRepeatsArray dla $length")
    printTimeMilis(countRepeatsArray(array,0,array.length,array(0)))

    println(s"countRepeatsFutureArray dla $length")
    printTimeMilis( countRepeatsFutureArray(array,array(0)))

    println()
    
  }

  def testCountReps(): Unit ={
    val sizes = 1.to(7).map(x => 3*Math.pow(10, x).toInt)
    for(size<- sizes)
      printTestRepResults(size)

  }

  def testFib(n:Int): Unit ={


    println(s"Fib($n)")
    printTimeMilis(fib(n))

    println(s"fibiter($n)")
    printTimeMilis(fibiter(n))

    println(s"fibFuture($n)")
    printTimeMilis(fibFuture(n))

    println()
    
  }


  def testFibFunctions(maxElem:Int): Unit ={
    var i=4
    while (i<maxElem)
      testFib(i)
      i+=10
  }

  def main(args: Array[String]): Unit ={
    //testCountReps()
      testFibFunctions(50)
    //[testQuickSort()

  }
}
