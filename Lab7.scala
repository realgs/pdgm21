package main.paradygmaty

import main.paradygmaty.Fibonacci.*
import main.paradygmaty.Utils.*
import main.paradygmaty.Matrix.*
import main.paradygmaty.Quick.*
import main.paradygmaty.SumOfTree.*
//thread pool
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object MainParallel extends App {


  //Szczegółowa analiza wyników wszystkich testów
  //znajduje się na dole tego pliku

  // TESTY SORTOWANIA QUICKSORT

  val listToSort = Array(4, 1, 7, 3, 96, 123, 12, 12, -2, 5, 0, 1, 5, -1, 7, 8)
  val listTwo = listToSort.clone()
  val listThree = listToSort.clone()

  printTimeMs(quickSort(listToSort), "Normal Quick")
  printArray(listToSort)
  println(Array(-2, -1, 0, 1, 1, 3, 4, 5, 5, 7, 7, 8, 12, 12, 96, 123).sameElements(listToSort))
  printTimeMs(parallelQuickort(listTwo), "QuickSort Parallel")
  printArray(listTwo)
  println(Array(-2, -1, 0, 1, 1, 3, 4, 5, 5, 7, 7, 8, 12, 12, 96, 123).sameElements(listTwo))
  printTimeMs(doubleQuickSort(listThree), "Double QuickSort")
  printArray(listThree)
  println(Array(-2, -1, 0, 1, 1, 3, 4, 5, 5, 7, 7, 8, 12, 12, 96, 123).sameElements(listThree))

  /* Dla tego testu kontrolowanego, otrzymano wyniki:
    Normal Quick, Time: 16 ms
    -2, -1, 0, 1, 1, 3, 4, 5, 5, 7, 7, 8, 12, 12, 96, 123
    true
    QuickSort Parallel, Time: 70 ms
    -2, -1, 0, 1, 1, 3, 4, 5, 5, 7, 7, 8, 12, 12, 96, 123
    true
    Double QuickSort, Time: 1 ms
    -2, -1, 0, 1, 1, 3, 4, 5, 5, 7, 7, 8, 12, 12, 96, 123
    true
  */

  println()
  println()

  val sortSize = 100000000
  val arraty = generateRandomArray(sortSize)
  val arratyCopy = arraty.clone()
  val arratyCopy2 = arraty.clone()

  printTimeMs(quickSort(arraty), "Normal Quick")
  //printArray(arraty)

  //wykomentowane, za długi czas oczekiwania, w porównaniu do innych "OutOfCierpliwośćException"

  //printTimeMs(parallelQuickort(arratyCopy), "parallel QuickSort")
  //printArray(arratyCopy)
  printTimeMs(doubleQuickSort(arratyCopy2), "Double QuickSort")


  println()
  println()

  //TESTY ZNAJDOWANIE NAJWIEKSZEJ LICZBY W MACIERZY

  val matrix = List(List(1.2, 4.33, -1, 2.33), List(5.12, 54.123, 4, -123), List(12, 43, -13.1, -23))

  print2DMatrix(matrix)
  println()
  println()

  println(printTimeMs(findMaxInMatrix(matrix), "Normal"))
  println(printTimeMs(findMaxInMatrixFuture(matrix), "Future"))
  println(printTimeMs(findMaxInMatrixQuadPowerFuture(matrix), "Quad Power"))

  /*
    Wyniki tego testu kontrolowanego:
    Normal, Time: 2 ms
    54.123
    Future, Time: 6 ms
    54.123
    Quad Power, Time: 7 ms
    54.123

  */

  println()
  println()
  val matrix2 = randomMatrixOfDim(10, 30, 0, 5)
  //print2DMatrix(matrix2)

  println(printTimeMs(findMaxInMatrix(matrix2), "Normal"))
  println(printTimeMs(findMaxInMatrixFuture(matrix2), "Future"))
  println(printTimeMs(findMaxInMatrixQuadPowerFuture(matrix2), "Quad Power"))



  println()
  println()

  //TESTOWANIE SUMY DRZEWA INT

  val testTree = Node(81,Node(46,Node(15,Empty,Empty),Node(54,Empty,Empty)),Node(61,Node(46,Empty,Empty),Node(79,Empty,Empty)))
  val testTree1 = Node(47,Node(4,Node(33,Empty,Empty),Node(46,Empty,Empty)),Node(24,Node(42,Empty,Empty),Node(81,Empty,Empty)))
  val testTree3 = Node(9,Node(86,Node(1,Empty,Empty),Node(79,Empty,Empty)),Node(77,Node(31,Empty,Empty),Node(71,Empty,Empty)))

  println(normalSumOfTree(testTree) == 382)
  println(normalSumOfTree(testTree1) == 277)
  println(normalSumOfTree(testTree3) == 354)

  println(sumTreeParallel(testTree) == 382)
  println(sumTreeParallel(testTree1) == 277)
  println(sumTreeParallel(testTree3) == 354)


  val sumDepth = 20
  val tree = generateTree(sumDepth)

  println(tree)
  println("Dla "+sumDepth)
  println(printTimeMs(normalSumOfTree(tree), "normal sum"))
  println(printTimeMs(sumTreeParallel(tree), "parallel sum"))




  println()
  println()


  //TESTY FIBONACCI

  val num = 20
  println(s"Dla $num")
  //Wykomentowane z powodów testów dużych liczb
  println(printTimeMs(fibonacciNormal(num), "Normal"))
  println(printTimeMs(fibonacciTailrec(num), "Tailrec"))
  println(printTimeMs(fibLazy(num), "lazy"))
  println( printTimeMs(fibonacciDoubleTailParallel(num), "Dual tail"))
  println(printTimeMs(fibonacciParallelFork(num), "Parallel"))

  // Implementacje oznaczam jako deprecated, lepiej użyć dual tailrec
  //Bardziej wytrzymały na stackOverflow
  println(printTimeMs(fibonacciForkTail(num), "forkTailrec"))

  //Nieistniejące już, albo wykomentowane
  //Mniej efektywne od używanch aktualnie teraz:
  //println(printTimeMs(FibonacciRecursiveLazy(num), "Recursive lazy"))
  //println(printTimeMs(fibonacciParallel(num), "Parallel"))




  /*

 Omówienie szukania maxa w Macierzy:

  Im większa liczba wierszy i kolumn tym bardziej wysuwa się na
  prowadzenie algorytm z użyciem future i quad power np.

  Dla 20 wierszy i 100 kolumn
    Normal, Time: 1 ms
    Future, Time: 58 ms
    Quad Power, Time: 8 ms
  Dla 20 wierszy i 1000 kolumn
    Normal, Time: 4 ms
    Future, Time: 57 ms
    Quad Power, Time: 8 ms
  Dla 20 wierszy i 10000 kolumn
    Normal, Time: 26 ms
    Future, Time: 72 ms
    Quad Power, Time: 10 ms
  Dla 200 wierszy i 10000 kolumn
    Normal, Time: 44 ms
    Future, Time: 73 ms
    Quad Power, Time: 16 ms
  Dla 200 wierszy i 100000 kolumn
    Normal, Time: 300 ms
    Future, Time: 178 ms
    Quad Power, Time: 103 ms
  Dla liczby wierszy 2000 i 10000 kolumn
    Normal, Time: 225 ms
    Future, Time: 176 ms
    Quad Power, Time: 79 ms

  Dla małej ilości wierszy i kolum lepszy jest zwykły algorytm
  Przy więszej lepsze wyniki są dla algorytmów ze zrównolegleniem,
  Jest to wynikiem tworzenia Future i przenoszenia wyników działań
  oraz czekania na rozpoczęcia i zakonczenia operacji.

 Omówienie sortowania poprzez Quicksort

  10
  Normal Quick, Time: 0 ms
  parallel QuickSort, Time: 108 ms
  Double QuickSort, Time: 0 ms

  100
  Normal Quick, Time: 4 ms
  parallel QuickSort, Time: 158 ms
  Double QuickSort, Time: 4 ms

  Dla 1000
  Normal Quick, Time: 8 ms
  Parallel QuickSort, Time: BRAK akceptowalnego wyniku, OutOfCierpliwośćException
  Double QuickSort, Time: 102 ms

  10000
  Normal Quick, Time: 12 ms
  Double QuickSort, Time: 104 ms

  100000
  Normal Quick, Time: 33 ms
  Double QuickSort, Time: 108 ms


  Double quick zaczyna wygrywac/ remisować
  1000000
  Normal Quick, Time: 132 ms,  Time: 143 ms
  Double QuickSort, Time: 147 ms, Time: 140 ms


  Powyżej 10000000 jest zwycięscą
  Normal Quick, Time: 1024 ms
  Double QuickSort, Time: 940 ms

  100000000
  Normal Quick, Time: 7887 ms, 7632 ms
  Double QuickSort, Time: 6420 ms, 4416 ms

  Wniosek: Dla mniejszych sortowań efektywniej jest użyć
  zwykłego algortymu. Dla większych > 1000000, należy
  użyć Double QuickSort, który będzie efektywniejszy.
  Parallel QuickSort okazał się nieefektywny, jest to efekt,
  tworzenia za dużej ilości future, prawdopodobnie zastosowanie
  RecursiveTask, albo głębokości do jakiej powinnien się dzielić,
  zwiększyłoby efektywność (tak jak zrobiłem z Fibonaccim).
  Tak samo jak w maxInMatrix powodem było, ciągłe tworzenia
  Future i przenoszenia wyników działań oraz czekania na
  rozpoczęcia i zakonczenia operacji.


  Omówienie sumOfTree:
  Dla 1
  normal sum, Time: 0 ms
  parallel sum, Time: 1 ms
  Dla 10
  normal sum, Time: 0 ms
  parallel sum, Time: 0 ms
  Dla 15
  normal sum, Time: 2 ms
  parallel sum, Time: 0 ms
  Dla 20
  normal sum, Time: 9 ms
  parallel sum, Time: 4 ms
  Dla 25
  normal sum, Time: 117 ms
  parallel sum, Time: 114 ms

  Widać, że algorytm zrównoleglony
  lepiej sobie radzi, dla średniej
  wielkości drzew. Dla małej i dużej
  wielkości, jest porównywalny z normalnym

Omówienie Fibonacci:

  Dla 0
  Normal, Time: 4 ms
  Tailrec, Time: 0 ms
  lazy, Time: 0 ms
  Dual tail, Time: 81 ms
  Parallel, Time: 0 ms
  forkTailrec, Time: 0 ms

  Dla 10
  Normal, Time: 3 ms
  Tailrec, Time: 0 ms
  lazy, Time: 4 ms
  Dual tail, Time: 70 ms
  Parallel, Time: 4 ms
  forkTailrec, Time: 0 ms

  Dla 20
  Normal, Time: 4 ms
  Tailrec, Time: 0 ms
  lazy, Time: 5 ms
  Dual tail, Time: 85 ms
  Parallel, Time: 4 ms

  Dla 20
  Normal, Time: 4 ms
  Tailrec, Time: 0 ms
  lazy, Time: 4 ms
  Dual tail, Time: 85 ms
  Parallel, Time: 4 ms
  forkTailrec, Time: 0 ms

  Dla 40
  Normal, Time: 808 ms
  Tailrec, Time: 0 ms
  lazy, Time: 5 ms
  Dual tail, Time: 75 ms
  Parallel, Time: 299 ms

  Normal, Time: 808 ms
  Tailrec, Time: 0 ms
  lazy, Time: 5 ms
  Dual tail, Time: 75 ms
  Parallel, Time: 299 ms
  forkTailrec, Time: 0 ms

  Dla 100 (Normal oraz Parallel wypadły z gry, za długi czas oczekiwania):
  Normal, Time: ==
  Tailrec, Time: 0 ms
  lazy, Time: 8 ms
  Dual tail, Time: 104 ms
  Parallel, Time: ==
  forkTailrec, Time: 0 ms

  Dla 1000 Tailrec czayna pokazywać pierwsze oznaki słabości (4ms)
  Tailrec, Time: 4 ms
  lazy, Time: 12 ms
  Dual tail, Time: 97 ms
  forkTailrec, Time: 1 ms

  Dla 10000 ForkTailrec doznał stackOverflow
  Tailrec, Time: 16 ms
  lazy, Time: 48 ms
  Dual tail, Time: 127 ms

  Dla 100000
  Tailrec, Time: 283 ms
  lazy, Time: 565 ms
  Dual tail, Time: 434 ms

  Dla 1000000 Lazy list doszło do swoich granic w tej konfiguracji: Java heap space
              Dual Tail wygrywa w porównaniu do zwykłego odpowiednika
  Tailrec, Time: 21072 ms
  lazy, java.lang.OutOfMemoryError
  Dual tail, Time: 17944 ms

  Dla 1500000 Kolejna wygrana dual taila
  Tailrec, Time: 35847 ms
  Dual tail, Time: 34472 ms

  Tak jak w poprzednich Dla bardzo dużych liczb,
    wydajniejsze okazało się dzielenie zadań.
    Dual tail jest wydajniejszy, poczas gdy
    dla mniejszych tailrec oraz lazy okazują się
    lepsze. Co ciekawe Dla małych liczb <40.
    Parallel okazał się (nawet) bardzo efektywny, lepszy
    od swojego normalnego odpowiednika oraz nawet lazy.

  */


 //W celu testów i utrzymania głównego wątka działającego
 Thread.sleep(10000)
}

