import MatrixSum.*
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import QuickSort.*
import MergeSort.*
import Factorial.*
import TreeSum.*

object Main extends App{

  //utils
  def printTestResults[A](test: => A): A =
    val startTime = System.nanoTime()
    val result = test
    val endTime = System.nanoTime()
    println("Time:" + (endTime - startTime) + " ns")
    result

  val r = scala.util.Random
  def generateList(length: Int): List[Int] =
    def addElement(toAdd: Int, list: List[Int]): List[Int] =
      if toAdd == 0 then list else addElement(toAdd-1, r.nextInt()::list)
    addElement(length, List())

  def generateRandomIntArray(size: Int, min: Int, max: Int): Array[Int] =
    Array.fill(size)(r.between(min, max))



  //test sum of matrix
  println (" ! Test sum of matrix !\n")

  //test1
  val matrix = List(List(2, 2, 2, 2), List(3, 3, 3, 3), List(4, 4, 4, 4))
  println("matrix:")
  printMatrix(matrix)
  println()

  println("Test normal")
  println(printTestResults(sumMatrixElements(matrix)))
  println()

  println("Test future")
  println(printTestResults(SumMatrixElementsFuture(matrix)))
  println()

  println("Test parallel")
  println(printTestResults(sumMatrixElementsParallel(matrix)))
  println()

  //test2
  val matrix2 = List(List(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), List(3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), List(4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), List(4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), List(4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), List(4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2))
  println("matrix:")
  printMatrix(matrix2)
  println()

  println("Test normal")
  println(printTestResults(sumMatrixElements(matrix2)))
  println()

  println("Test future")
  println(printTestResults(SumMatrixElementsFuture(matrix2)))
  println()

  println("Test parallel")
  println(printTestResults(sumMatrixElementsParallel(matrix2)))
  println()
  println()

  /*
      ! Test sum of matrix !

      matrix:
      2 2 2 2
      3 3 3 3
      4 4 4 4

      Test normal
      Time:2172900 ns
      36

      Test future
      Time:73816600 ns
      36

      Test parallel
      Time:7886200 ns
      36

      matrix:
      2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
      3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2
      4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2
      4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2
      4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2
      4 4 4 4 2 2 2 2 2 2 2 2 2 2 2 2

      Test normal
      Time:206200 ns
      228

      Test future
      Time:567900 ns
      228

      Test parallel
      Time:119100 ns
      228

  */


  //test sum of tree nodes
  println (" ! Test sum of tree nodes !")

  //test1
  val sumDepth = 10
  val tree = generateBinaryTree(sumDepth)
  println("  for depth: " + sumDepth + "\n")

  println("Test normal")
  println(printTestResults(treeElementsSum(tree)))
  println()

  println("Test future")
  println(printTestResults(treeElementsSumFuture(tree, 15)))
  println()

  println("Test parallel")
  println(printTestResults(treeElementsSumParallel(tree, 15)))
  println()
  println()

  //test2
  val sumDepth2 = 20
  val tree2 = generateBinaryTree(sumDepth2)
  println("  for depth: " + sumDepth2 + "\n")

  println("Test normal")
  println(printTestResults(treeElementsSum(tree2)))
  println()

  println("Test future")
  println(printTestResults(treeElementsSumFuture(tree2, 25)))
  println()

  println("Test parallel")
  println(printTestResults(treeElementsSumParallel(tree2, 25)))
  println()
  println()

  /*

    ! Test sum of tree nodes !
      for depth: 10

    Test normal
    Time:572600 ns
    504.0021175146103

    Test future
    Time:46800 ns
    504.0021175146103

    Test parallel
    Time:44800 ns
    504.0021175146103

      for depth: 20

    Test normal
    Time:13059400 ns
    524659.0251210332

    Test future
    Time:13030300 ns
    524659.0251210332

    Test parallel
    Time:12752300 ns
    524659.0251210332

  */



  //test quicksort
  println (" ! Test quicksort !\n")

  //test1
  val list1 = Array(1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13)
  println("Test normal")
  printArray(list1)
  printTestResults(quicksort(list1))
  println()

  val list2 = Array(1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13)
  println("Test future")
  printArray(list2)
  printTestResults(futureQuicksort(list2))
  println()

  val list6 = Array(1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13)
  println("Test future 2")
  printArray(list6)
  printTestResults(futureQuicksort2(list6))
  println()

  val list5 = Array(1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13)
  println("Test parallel")
  printArray(list5)
  printTestResults(parallelQuicksort(list5))
  println()

  //test2
  val list3 = generateRandomIntArray(100, -1000, 1000)
  println("Test normal")
  printTestResults(quicksort(list3))
  println()

  val list4 = generateRandomIntArray(100, -1000, 1000)
  println("Test future")
  printTestResults(futureQuicksort(list4))
  println()

  val list7 = generateRandomIntArray(100, -1000, 1000)
  println("Test future 2")
  printTestResults(futureQuicksort2(list7))
  println()

  val list8 = generateRandomIntArray(100, -1000, 1000)
  println("Test parallel")
  printTestResults(parallelQuicksort(list8))
  println()
  println()

  /*

    ! Test quicksort !

      Test normal
      1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13
      Time:36300 ns

      Test future
      1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13
      Time:3437600 ns

      Test future 2
      1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13
      Time:692100 ns

      Test parallel
      1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13
      Time:738200 ns

      Test normal
      Time:62300 ns

      Test future
      Time:28567200 ns

      Test future 2
      Time:571400 ns

      Test parallel
      Time:57700 ns

  */




  //test factorial
  println (" ! Test factorial !\n")

  //test 1
  var number1 = 10
  println("Test normal")
  printTestResults(countFatorial(number1, 1))
  println(countFatorial(number1, 1))
  println()

  println("Test future")
  printTestResults(countFactorialFuture(number1))
  println(countFactorialFuture(number1))
  println()

  println("Test parallel")
  printTestResults(countFatorialParallel(number1, 1))
  println(countFatorialParallel(number1, 1))
  println()

  //test 2
  var number2 = 30
  println("Test normal")
  printTestResults(countFatorial(number2, 1))
  println(countFatorial(number2, 1))
  println()

  println("Test future")
  printTestResults(countFactorialFuture(number2))
  println(countFactorialFuture(number2))
  println()

  println("Test parallel")
  printTestResults(countFatorialParallel(number2, 1))
  println(countFatorialParallel(number2, 1))
  println()
  println()

  /*

    ! Test factorial !

      Test normal
      Time:1052700 ns
      3628800

      Test future
      Time:5600500 ns
      3628800

      Test parallel
      Time:4540200 ns
      3628800

      Test normal
      Time:147700 ns
      265252859812191058636308480000000

      Test future
      Time:231700 ns
      265252859812191058636308480000000

      Test parallel
      Time:146900 ns
      265252859812191058636308480000000

  */


  //test merge sort
  println (" ! Test merge sort !\n")

  //test 1
  val listToSort = List(1, 5, 3, 7, 4, 6, 8, 9, 10, 3, 0, -2, 23, 14, 6, 1, 12, 13)

  println("Test normal")
  printTestResults(mergesort((a:Int, b:Int) => a < b, listToSort))
  println(mergesort((a:Int, b:Int) => a < b, listToSort))
  println()

  println("Test parallel")
  printTestResults(mergesortParallel((a:Int, b:Int) => a < b, listToSort))
  println(mergesortParallel((a:Int, b:Int) => a < b, listToSort))
  println()

  //test 2
  val listToSort2 = generateList(100)

  println("Test normal")
  printTestResults(mergesort((a:Int, b:Int) => a < b, listToSort2))
  println()

  println("Test parallel")
  printTestResults(mergesortParallel((a:Int, b:Int) => a < b, listToSort2))

  /*
    ! Test merge sort !

    Test normal 18
    Time:2784200 ns
    List(-2, 0, 1, 1, 3, 3, 4, 5, 6, 6, 7, 8, 9, 10, 12, 13, 14, 23)

    Test parallel 18
    Time:1571900 ns
    List(-2, 0, 1, 1, 3, 3, 4, 5, 6, 6, 7, 8, 9, 10, 12, 13, 14, 23)

    Test normal 100
    Time:1048000 ns

    Test parallel 100
    Time:1117300 ns

  */

  /* WNIOSKI

    Suma wartości węzłów w drzewie:

    Zarówno zrównoleglenie 1 sposobem (future) jak i drugim (parallel) pozwala na przyspieszenie obliczenia sumy,
    przy czym dla wszystkich wykonanych testów parallel działa szybciej niż future. Dla małej i dużej głębokości drzewa
    czas obliczenia sumy dla wszystkich 3 sposobów różni się jednak niewiele i nie zależy od głębokości drzewa.

    Suma elementów macierzy:

    Dla małego rozmiaru macierzy szybciej działa zwyczajny algorytm, co jest związane z koniecznością wykonania
    dodatkowych operacji przy tworzeniu wątków. Wraz ze wzrostem pola macierzy coraz bardziej opłaca się
    zastosowanie algorytmów zrównoleglonych, przy czym zrównoleglenie sposobem parallel daje lepszy czas niż future.

    Obliczanie silni:

    Dla obliczenia silni z niewielkiej liczby zdecydowanie bardziej opłaca się zastosowanie zwykłego algorytmu. Od
    obliczenia silni z ok. liczby 20 coraz bardziej opłaca się zastosownie algorytmów zrównoleglonych. Parallel
    daje szybszy rezultat od liczby ok. 25, algorytm future działa wolniej i jest lepszy od zwyczajnego od ok liczby 25.

    Merge Sort:

    Dla tego rodzaju sortowania opłaca się zastosować zrównoleglenie parallel już dla małej długości list. Dla małych i
    średnich list nie są to znaczne różnice, ale stale wersja zrównoleglona daje wynik szybciej.

    Quick Sort:

    Dla sortowania tablic o niewielkiej długości bardziej opłaca się użyć zwykłego algorytmu. Zastosowanie algorytmu
    future w takim wydaniu nie ma sensu, ponieważ nigdy nie osiąga on rezultatu lepszego niż zwykły algorytm. Im dłuższa
    tablica tym przewaga algorytmów future2 i parallel się zwiększa, przy czym parallel działa zawsze znacznie szybciej
    od future2. Problem zastosowania zrównoleglenia polega na konieczności ciągłego tworzenia wątków, przenoszenia wyników
    i czekania na zakończenie operacji.
  */
}
