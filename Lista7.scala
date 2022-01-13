import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Lista7 {

  def time[T](f: => T): Unit = {
      val start = System.nanoTime()
      val fTime = f
      println("Czas "+ (System.nanoTime() - start))
    }


  //1)
  def fib(n: Int): Int =
    n match
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)

  def fibTail(n: Int): Int =
    @tailrec
    def fibIn(n: Int, a: Int, b: Int): Int =
      n match
        case 0 => a
        case 1 => b
        case _ => fibIn(n - 1, b, a + b)

    fibIn(n, 0, 1)

  def fibPar(n: Int): Int = {
    n match
      case 0 => 0
      case 1 => 1
      case _ =>
        val f1 = Future(fib(n - 1))
        val f2 = Future(fib(n - 2))
        val f1Result = Await.result(f1, Duration.Inf)
        val f2Result = Await.result(f2, Duration.Inf)
        f1Result + f2Result
  }


  //2)
  def mergeSort(list: List[Int]): List[Int] =
    list match {
      case Nil => Nil
      case head :: Nil => List(head)
      case _ =>
        val (left, right) = list splitAt list.length / 2
        merge(mergeSort(left), mergeSort(right))
    }

  def mergeSortPar(list: List[Int]): List[Int] =
    list match {
      case Nil => Nil
      case head :: Nil => List(head)
      case _ =>
        val (left, right) = list splitAt list.length / 2
        val leftF = Future(mergeSort(left))
        val rightF = Future(mergeSort(right))
        val resultLeft = Await.result(leftF, Duration.Inf)
        val resultRight = Await.result(rightF, Duration.Inf)
        merge(resultLeft, resultRight)
    }

  def merge(list1: List[Int], list2: List[Int]): List[Int] =
    (list1, list2) match {
      case (Nil, _) => list2
      case (_, Nil) => list1
      case (head1 :: tail1, head2 :: tail2) =>
        if (head1 < head2) head1 :: merge(tail1, list2)
        else head2 :: merge(list1, tail2)
    }


  def main(args: Array[String]): Unit = {
    println("Fibonacci \n4: ")

    print("Bez zrównoleglenia: ")
    time(fib(4))

    print("Bez zrównoleglenia (ogonowa): ")
    time(fibTail(4))

    print("Zrównoleglenie: ")
    time(fibPar(4))

    println("\n20: ")

    print("Bez zrównoleglenia: ")
    time(fib(20))

    print("Bez zrównoleglenia (ogonowa): ")
    time(fibTail(20))

    print("Zrównoleglenie: ")
    time(fibPar(20))

    println("\n40: ")
    print("Bez zrównoleglenia: ")
    time(fib(40))

    print("Bez zrównoleglenia (ogonowa): ")
    time(fibTail(40))

    print("Zrównoleglenie: ")
    time(fibPar(40))

   //Dla mniejszych liczb lepsze jest rozwiązanie jednowątkowe niż wielowątkowe.
   //Przy większych liczbach wielowątkowość wypada lepiej
   //We wszystkich wypadkach użycie rekursji ogonowej wypada najlepiej pod względem czasu
   //4:
   //bez zrównoleglenia: 20 000- 30 000   ogonowa: 7 000-10 000   zrównoleglenie: ponad 70 000 000
   //20:
   //bez zrównoleglenia: 250 000- 500 000   ogonowa: 5 000-8 000   zrównoleglenie: 1 700 000-1 800 000
   //40:
   //bez zrównoleglenia: około 530 000 000   ogonowa: 7 000-8 000   zrównoleglenie: około 320 000 000



    print("\nMergesort \n100 elementów")
    val list = List.fill(100)(1000).map(scala.util.Random.nextInt)
    print("\nBez zrównoleglenia: ")
    time(mergeSort(list))
    print("Zrównoleglenie: ")
    time(mergeSortPar(list))

    print("\n50 elementów")
    val list2 = List.fill(50)(1000).map(scala.util.Random.nextInt)
    print("\nBez zrównoleglenia: ")
    time(mergeSort(list2))
    print("Zrównoleglenie: ")
    time(mergeSortPar(list2))

    print("\n1 000 elementów")
    val list1k = List.fill(1000)(1000).map(scala.util.Random.nextInt)
    print("\nBez zrównoleglenia: ")
    time(mergeSort(list1k))
    print("Zrównoleglenie: ")
    time(mergeSortPar(list1k))

    print("\n10 000 elementów")
    val list10k = List.fill(10000)(1000).map(scala.util.Random.nextInt)
    print("\nBez zrównoleglenia: ")
    time(mergeSort(list10k))
    print("Zrównoleglenie: ")
    time(mergeSortPar(list10k))

    //Przewaga zrównoleglenia dla dużych danych już od 10 tys. elementów (niemal 2 razy krótszy czas)
    //Dla małych danych np 50 elementów lepsze rozwiązanie jednowątkowe
    //50 elementów:
    //bez zrównoleglenia: 200 000- 400 000   zrównoleglenie: 300 000- 400 000
    //100 elementów:
    //bez zrównoleglenia: około 2 000 000   zrównoleglenie: około 1 000 000
    //1000 elementów:
    //bez zrównoleglenia: ponad 2 000 000  zrównoleglenie:  2 000 000 - 5 000 000
    //10 000 elementów:
    //bez zrównoleglenia:  8 000 000- 10 000 000  zrównoleglenie: 5 000 000 -6 000 000

  }
}

