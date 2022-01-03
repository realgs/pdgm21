import sun.security.util.Length

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random
object L7 {





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



  def testRep(length:Int): Unit ={
    val list =generateList(length,20000)
    val array = createArray(length,20000)

    var time1 = System.nanoTime()
    countRepeatsList(list,list.head)
    var time2 = (System.nanoTime()-time1)/1000/1000
    println(s"countRepeatsList dla $length elementow wykonano w $time2 ms")


    time1 = System.nanoTime()
    countRepeatsFutureList(list,list.head)
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"countRepeatsFutureList dla $length elementow wykonano w $time2 ms")

    time1 = System.nanoTime()
    countRepeatsFuture2List(list,list.head)
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"countRepeatsFuture2List dla $length elementow wykonano w $time2 ms")

    println()

    time1 = System.nanoTime()
    countRepeatsArray(array,0,array.length,array(0))
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"countRepeatsArray dla $length elementow wykonano w $time2 ms")

    time1 = System.nanoTime()
    countRepeatsFutureArray(array,array(0))
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"countRepeatsFutureArray dla $length elementow wykonano w $time2 ms")

    println("\n\n\n")
  }

  def testCountReps(): Unit ={
    val sizes = 1.to(7).map(x => 3*Math.pow(10, x).toInt)
    for(size<- sizes)
      testRep(size)

  }

  def testFib(n:Int): Unit ={
    var time1 = System.nanoTime()
    var time2=System.nanoTime()
    time1 = System.nanoTime()
    fibiter(n)
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"fibiter($n) wykonano w $time2 ms")

    time1 = System.nanoTime()
    fib(n)
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"fib($n) wykonano w $time2 ms")

    time1 = System.nanoTime()
    fibFuture(n)
    time2 = (System.nanoTime()-time1)/1000/1000
    println(s"fibFuture($n) wykonano w $time2 ms")

    println("\n\n\n")

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


  }
}
