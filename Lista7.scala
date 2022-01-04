import com.sun.org.apache.xalan.internal.lib.ExsltDatetime.time

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps
import math.Numeric.Implicits.infixNumericOps


object Lista7 {

  def normalFib(num: Int): Long =
    if (num < 2) num
    else normalFib(num - 1) + normalFib(num - 2)


  def tailrecFib(num: Int): Long = {
    @scala.annotation.tailrec
    def fibTail(num: Long, next: Long, sum: Long): Long =
      if (num == 0) sum
      else fibTail(num - 1, sum + next, next)

    fibTail(num, 1, 0)
  }


  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val f1 = Future {
      taskA
    }
    val f2 = Future {
      taskB
    }
    val res1 = Await.result(f1, Duration.Inf)
    val res2 = Await.result(f2, Duration.Inf)
    (res1, res2)

  def parallelFib(num: Int): Long =
    if (num < 40) {
      if (num < 2) num
      else normalFib(num - 1) + normalFib(num - 2)
    } else {
      val (fib1, fib2) = parallel(parallelFib(num - 1), parallelFib(num - 2))
      fib1 + fib2
    }

  def isPrimeNumber(num: Long): Boolean =
    @tailrec
    def isPrimeTail(num: Long, lim: Long): Boolean =
      if (lim == 1) true
      else if (num % lim == 0) false
      else isPrimeTail(num, lim - 1)

    if (num > 0) isPrimeTail(num, num - 1)
    else false


  def isPrimeNumber2(num: Long): Boolean =
    @tailrec
    def isPrimeTail2(num: Long, lim: Long): Boolean =
      if ((lim == 1) || (lim == 0)) true
      else if (num % lim == 0) false
      else isPrimeTail2(num, lim - 2)

    if ((num >= 0) && isPrimeTail2(num, num - 1) && isPrimeTail2(num, num - 2)) true
    else false


  def isPrimeNumberParallel(num: Long): Boolean =
    @tailrec
    // co w takiej sytuacji spytać Doktora
    def isPrimeTail2(num: Long, lim: Long): Boolean =
      if ((lim == 1) || (lim == 0)) true
      else if (num % lim == 0) false
      else isPrimeTail2(num, lim - 2)

    if (num < 0) false
    else {
      val (fib1, fib2) = parallel(isPrimeTail2(num, num - 1), isPrimeTail2(num, num - 2))
      fib1 && fib2
    }


  def swap(array: Array[Int], pos1: Int, pos2: Int): Unit = {
    val temp = array(pos1)
    array(pos1) = array(pos2)
    array(pos2) = temp
  }


  def quicksort(a: Array[Int], low: Int, hi: Int): Unit = {
    if (low < hi) {
      val p = partition(a, low, hi)
      quicksort(a, low, p - 1)
      quicksort(a, p + 1, hi)
    }
  }

  def partition(subArray: Array[Int], low: Int, hi: Int): Int = {
    val pivot = hi
    var i = low
    for (
      j <- low to hi
      if subArray(j) < subArray(pivot)
    ) {
      swap(subArray, i, j); i += 1
    }

    swap(subArray, i, pivot)
    i
  }

  def quicksortParallel(a: Array[Int], low: Int, hi: Int): Unit = {
    if (low < hi) {
      val p = partition(a, low, hi)
      if (hi - low < 20000) {
        quicksort(a, low, p - 1)
        quicksort(a, p + 1, hi)
      } else {
        val (f1, f2) = parallel(quicksortParallel(a, low, p - 1), quicksortParallel(a, p + 1, hi))
      }
    }
  }

  def createRandomArray(range: Int, length: Int): Array[Int] =
    Array.fill(length)(scala.util.Random.nextInt(range))


  def printArray(a: Array[Int]): Unit = {
    println()
    for (i <- 0 until a.length - 1)
      print(a(i) + " ")
  }

  def main(args: Array[String]): Unit = {

    // FIBO

    println("\nfibo 41 liczba")
    var time = System.nanoTime()
    //println(normalFib(50))
    println("czas rekurencji zwyklej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    //println(tailrecFib(50))
    println("czas rekurencji ogonowej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    //println(parallelFib(50))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)


    println("\nfibo 50 liczba")
    time = System.nanoTime()
    //println(normalFib(50))
    println("czas rekurencji zwyklej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    //println(tailrecFib(50))
    println("czas rekurencji ogonowej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    //println(parallelFib(50))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)

    // przedział oznacza że wyniki znacząco mogły się różnić. Około oznacza wynik który nie odbiega w znaczącym stopniu od większości otrzymywanych wyników


    // Wyniki działania ciągu fibo są zadziwiające. Absolutnie zdominowała rekursja ogonowa która przyćmiła obie rekursje nieogonowe
    // Dla mniejszych liczb wielowątkowość nie opłaca się nawet względem rekursji nieogonowej co widać na wynikach:
    // dla fib 20 i przejścia na jednowątkowość od 10:
    // no-rec:   2000000-3000000           rec:  15000-22000           parallel: 80000000-140000000
    // dla fib 40 i przejścia na jednowątkowość od 30:
    // no-rec:  około 516,337,452           rec:  około 23746           parallel: około 413,117,272   widzimy tu lekką poprawę na rzecz wielowątkowości, jednak nie
    //ma ona znaczenia w różnicy z rekursją ogonową
    // dla fib 50 i przejścia na jednowątkowość od 40
    // no-rec:  około 63359742153           rec: około 26261         parallel: około 22362378591
    // trzykrotne przyspieszenie dzięki wielowątkowości jest znaczące, jednak wciąż nie dorównuje rekursji ogonowej.

    // Wniosek: Parallel dla bardzo dużych liczb ma znaczenie. Warto jednak pamiętać, że nawet najlepszy programista nie pokona kompilatora, co pokazuje wyższość
    //rekursji ogonowej


    //test działania isprime i isprime2

    println(isPrimeNumber(13))
    println(isPrimeNumber2(13))

    println(!isPrimeNumber(36))
    println(!isPrimeNumber2(36))

    println(isPrimeNumber(991))
    println(isPrimeNumber2(991))


    // QUICKSORT

    val array = createRandomArray(100, 100)
    quicksort(array, 0, array.length - 1)
    printArray(array)


    val array2 = createRandomArray(100, 100)
    quicksortParallel(array2, 0, array2.length - 1)
    printArray(array2)


    println()


    val array3 = createRandomArray(100, 1000000)
    time = System.nanoTime()
    //quicksort(array3, 0, array3.length - 1)
    println("czas quicksortu: " + (System.nanoTime() - time).toString)


    val array4 = createRandomArray(100, 1000000)
    time = System.nanoTime()
    //quicksortParallel(array4, 0, array4.length - 1)
    println("czas quicksortu zrownoleglonego: " + (System.nanoTime() - time).toString)

    // Wyniki są przerażająco słabe dla quick sortu zrównoleglonego dla mniejszych liczb np dla długości 1000 10,000-40,000 nanosekund vs 3,000,000-5,000,000 nanosekund
    // Wyniki są również słabe jeśli dla mniejszych partycji tworzymy nowe wątki. Prowadzi to do wytworzenia ogromnej liczby wątków co:
    // - spowalnia program przy rozsądnej liczbie wątków
    // - zawiesza go na wieki w przypadku ogromnej liczby wątków

    // Parallel quick sort jest jednak znacznie szybszy dla liczb wielkości rzędu wielkości 1,000,000. Dla takiej długości array'u mamy wyniki 1,000,000-1,500,000 nanosekund
    // dla niezrównloleglonego vs 700,000-1,100,000 dla quicksortu zrównoleglonego.
    // wnioski:
    // quicksort ogromnych arrayów warto zrównoleglać, mniejszych zdecydowanie nie!


    // PRIME NUMBERS

    println("PRIME NUMBERS\n")
    time = System.nanoTime()
    println(isPrimeNumber(241271))
    println("czas rekurencji ogonowej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    println(isPrimeNumber2(241271))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    println(isPrimeNumberParallel(241271))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    println(isPrimeNumber(82589933))
    println("czas rekurencji ogonowej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    println(isPrimeNumber2(82589933))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    println(isPrimeNumberParallel(82589933))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)

    time = System.nanoTime()
    println(isPrimeNumber(2412710003))
    println("czas rekurencji ogonowej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    println(isPrimeNumber2(2412710003))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)
    time = System.nanoTime()
    println(isPrimeNumberParallel(2412710003))
    println("czas rekurencji zrównoleglonej: " + (System.nanoTime() - time).toString)

    // dla metody sprawdzającej czy funkcja jest liczbą pierwszę wyniki podzielone są na 3 grupy
    // no-rec:             split-rec:           parallel-rec:
    // dla liczby 241271
    // no-rec:  około 2,204,605          split-rec: około 2,607,099         parallel-rec: około 62,204,329
    // dla liczby 82589933
    // no-rec:  około 284,581,297         split-rec: około 284,118,320        parallel-rec: około 190,459,434
    // dla liczby 2412710003
    // no-rec:  około 7,452,415,220        split-rec: około 7,874,285,387       parallel-rec: około 4,178,202,551
    // Wszystkie wyniki są bardzo zbliżone do siebie, jako że jest tu mało miejsca na różnice(brak losowości ułożenia arrayu itp.)
    // Wnioski: W tej metodzie zrównoleglenie rzeczywiście ma znaczący wpływ i im większe liczby, tym większy ten wpływ powinien być. Przy małych liczbach jest
    // to kompletnie nieopłacalne co widać w 1 przykładzie
    // Wniosek ogólny: UWAGA NA LICZBĘ WĄTKÓW, nie chcemy mieć ich za dużo. Programowanie zrównoleglone moze zwiększać znacząco wydajność, szczególnie gdy mamy do
    // czynienia z łatwo rozbijalną rekursją. Jest ona pomocna tylko dla bardzo dużych liczb!!!!






  }
}
