import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Random

object List7 {

  //Funkcje pomocnicze

  def myMap[A, B](list: List[A], f: A => B): List[B] =
    list match
      case List() => List()
      case head :: tail => f(head) :: myMap(tail, f)

  def myReverse[A](list: List[A]): List[A] =
    @tailrec
    def reverseIter[A](reversedList: List[A], list: List[A]): List[A] =
      list match
        case Nil => reversedList
        case h :: t => reverseIter(h :: reversedList, t)

    reverseIter(List(), list)

  def flattenList[A](xss: List[List[A]]): List[A]=
    if xss==Nil then Nil
    else xss.head ::: flattenList(xss.tail)

  def quicksortSplit(list: List[Int], subLists: Int): List[List[Int]] =
    @tailrec
    def splitInner(queue: List[List[Int]], count: Int) : List[List[Int]] =
      queue match
        case h :: t =>
          if(count > 1) then
            val(left, right) = divideList(h, h.head)
            splitInner(queue:::List(left):::List(right), count - 1)
          else queue
        case Nil =>
          val(left, right) = divideList(list, list.head)
          splitInner(queue:::List(left):::List(right), count - 1)
    splitInner(List(), subLists)

  def generateList(range : BigInt): List[Int] =
    val r = new scala.util.Random
    @tailrec
    def generateListHelper(acc: List[Int], range: BigInt): List[Int] =
      if (range == 0) then acc
      else generateListHelper(r.nextInt(100) :: acc, range-1)
    generateListHelper(List(), range)

  //Quicksort
  def divideList(list: List[Int], pivot: Int): (List[Int], List[Int]) =
    @tailrec
    def divideHelper(list: List[Int], acc: (List[Int], List[Int])): (List[Int], List[Int]) =
      list match
        case Nil => acc
        case h :: t =>
          if (h < pivot) then divideHelper(t, (h :: acc._1, acc._2))
          else divideHelper(t, (acc._1, h :: acc._2))

    val (left, right) = divideHelper(list, (List(), List()))
    (myReverse(left), myReverse(right))

  def quickSort(list: List[Int]): List[Int] =
    list match
      case Nil => Nil
      case h :: Nil => list
      case h :: t =>
        val pivot = list.head
        val (left, right) = divideList(t, pivot)
        quickSort(left) ::: pivot :: quickSort(right)

  def parallelQuickSort(list: List[Int]): List[Int] =
    list match
      case Nil => Nil
      case h :: Nil => list
      case h :: t =>
        val pivot = list.head
        val (left, right) = divideList(list, pivot)
        val leftList = Future(quickSort(left))
        val rightList = Future(quickSort(right.tail))
        Await.result(leftList, Duration.Inf) ::: pivot :: Await.result(rightList, Duration.Inf)

  def parallelQuickSort2(list: List[Int]): List[Int] =
    val futures = for(lista <- quicksortSplit(list, 2)) yield Future{quickSort(lista)}
    flattenList(futures.map(Await.result(_, Duration.Inf)))

  def quickSortTail(list: List[Int]) =
    @tailrec
    def quickTailHelper(queue: List[List[Int]], acc: List[Int]): List[Int] =
      queue match
        case Nil => acc
        case head :: tail =>
          head match
            case Nil => quickTailHelper(tail, acc)
            case h :: t =>
              val (left, right) = divideList(t, h)
              if (left.isEmpty) then
                if (right.isEmpty) then quickTailHelper(tail, h :: acc)
                else quickTailHelper(right :: tail, h :: acc)
              else quickTailHelper(left :: List(h) :: right :: tail, acc)
    myReverse(quickTailHelper(List(list),Nil))

  def parallelQuickSortTail(list: List[Int]): List[Int] =
    list match
      case Nil => Nil
      case h :: Nil => list
      case h :: t =>
        val pivot = list.head
        val (left, right) = divideList(list, pivot)
        val leftList = Future(quickSortTail(left))
        val rightList = Future(quickSortTail(right.tail))
        Await.result(leftList, Duration.Inf) ::: pivot :: Await.result(rightList, Duration.Inf)

  def parallelQuickSortTail2(list: List[Int]): List[Int] =
    val futures = for(lista <- quicksortSplit(list, 2)) yield Future{quickSortTail(lista)}
    flattenList(myMap(futures, (Await.result(_, Duration.Inf))))

  //Drzewa
  //Typ danych dla drzew
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  def treeAverage(tree: BT[Int]): Double =
    def treeAverageHelper(tree: BT[Int], counter: Int): (Double, Double) =
      tree match
        case Empty => (0, 0)
        case Node(elem, left, right) =>
         val (leftSum, leftCounter) = treeAverageHelper(left, counter)
         val (rightSum, rightCounter) = treeAverageHelper(right, counter)
         (elem + leftSum + rightSum, 1 + leftCounter + rightCounter)
    val (sumOfElem, nodesNumber) = treeAverageHelper(tree, 0)
    sumOfElem / nodesNumber

  def treeAverageReturnNodesNum(tree: BT[Int]): (Double, Double) =
    def treeAverageHelper(tree: BT[Int], counter: Int): (Double, Double) =
      tree match
        case Empty => (0, 0)
        case Node(elem, left, right) =>
          val (leftSum, leftCounter) = treeAverageHelper(left, counter)
          val (rightSum, rightCounter) = treeAverageHelper(right, counter)
          (elem + leftSum + rightSum, 1 + leftCounter + rightCounter)
    val (sumOfElem, nodesNumber) = treeAverageHelper(tree, 0)
    (sumOfElem / nodesNumber, nodesNumber)

  def parallelTreeAverage(tree: BT[Int]): Double =
    tree match
      case Empty => -1
      case Node(elem, left, right) =>
        val leftAverage = Future{treeAverageReturnNodesNum(left)}
        val rightAverage = Future{treeAverageReturnNodesNum(right)}
        val result = for(left <- leftAverage; right <- rightAverage) yield (1/(left._2 + right._2 + 1))*((left._1*left._2) + (right._1*right._2) + elem)
        Await.result(result, Duration.Inf)


  def breadthBT[A] (tree: BT[A]): List[A] =
    def breadthTraversal(childQueue: List[BT[A]]): List[A] =
      childQueue match
        case Nil => Nil
        case Empty :: t => breadthTraversal(t)
        case Node(elem, left, right)::t => elem::breadthTraversal (t ::: List(left, right))
    breadthTraversal (List(tree))

  def generateTree(depth: Int): BT[Int] =
    val r = new scala.util.Random
    def generateTreeHelper(depth: Int):BT[Int] =
      if (depth >= 0) then Node(r.nextInt(100), generateTreeHelper(depth - 1), generateTreeHelper(depth - 1))
      else Empty
    generateTreeHelper(depth)

  //Podnoszenie do kwadratu elementow listy

  def splitList[A](list: List[A], subListsNum: Int): List[List[A]] =
    @tailrec
    def splitListHelper[A](list: List[A], acc: List[List[A]], elements: Int, remainder: Int) : List[List[A]] =
      if(list.isEmpty) then acc
      else
        var values = 0
        if(remainder > 0) then values = elements + 1
        else values = elements
        val headList: List[A] = list.take(values)
        val tailList : List[A]= list.drop(values)
        splitListHelper(tailList, headList :: acc, elements, remainder - 1)

    splitListHelper(list, Nil, list.length / subListsNum, list.length % subListsNum).reverse

  def squareList(list: List[Int]): List[Int] =
    @tailrec
    def squareListHelper(list: List[Int], acc: List[Int]): List[Int] =
      list match
        case Nil => acc
        case head :: tail => squareListHelper(tail, (head*head) :: acc)
    myReverse(squareListHelper(list, List()))

  def parallelSquareList(list: List[Int]): List[Int] =
    val futures = for(lista <- splitList(list, 6)) yield Future{squareList(lista)}
    flattenList(futures.map(Await.result(_, Duration.Inf)))

  //Lucas numbers
  def lucasNumberTail(range: Int): BigInt =
    @tailrec
    def lucasNumberHelper(range: Int, current: Int, next: Int): Int =
      if(range <= 0) then current
      else lucasNumberHelper(range -1, next, current + next)
    lucasNumberHelper(range, 2, 1)

  def lucasNumber(n: Int): BigInt =
    n match
    case 1 => 1
    case 0 => 2
    case _ => lucasNumber(n-2) + lucasNumber(n-1)

  def parallelLucasNumber(n: Int): BigInt =
    val first = Future(lucasNumber(n-2))
    val second = Future(lucasNumber(n-1))
    Await.result(first, Duration.Inf) + Await.result(second, Duration.Inf)

  def printElapsedTime[A](block: => A) = {
    val time1 = System.currentTimeMillis()
    block // call by name
    val time2= System.currentTimeMillis()
    println((time2 - time1) + " ms")
  }


  def main(args: Array[String]): Unit = {

    val list1 = generateList(10)
    val tree = generateTree(3)

    println("PRZYKLADOWE WYNIKI DZIALANIA: ")

    println("QUICK SORT")
    println(quickSort(list1))
    println(parallelQuickSort(list1))
    println( parallelQuickSort2(list1))
    println(quickSortTail(list1))
    println(parallelQuickSortTail(list1))
    println(parallelQuickSortTail2(list1))

    println("TREE AVERAGE")
    println(treeAverage(tree))
    println(parallelTreeAverage(tree))

    println("SQUARE LIST")
    println(squareList(list1))
    println(parallelSquareList(list1))

    println("LUCAS NUMBER")
    println(lucasNumber(5))
    println(parallelLucasNumber(5))

    //Generowanie list losowych elementow roznej dlugosci
    val lists = Array(generateList(10), generateList(100), generateList(1000), generateList(10000), generateList(100000), generateList(1000000), generateList(10000000), generateList(50000000))
    var number = 10

    //Generowanie drzew losowych elementow roznej glebokosci
    val trees = Array(generateTree(2), generateTree(16), generateTree(17), generateTree(18), generateTree(19), generateTree(20))
    val depths = Array(2, 16, 17, 18, 19, 20)

    //Tablica kolejnych liczb Lucasa do obliczenia
    val lucasNumbers = Array(10, 15, 20, 25, 30, 40)
    //****************************QUICK SORT*******************************
    println("\n***************QUICK SORT*********************************")

    for(i <- 0 to 4)
      println("\nNUMBER OF ELEMENTS: " + number+"\n")

      //Quick Sort bez zrownoleglania
      print("Time not parallel: ")
      printElapsedTime(quickSort(lists(i)))

      //Quick Sort ze wspolbieznoscia wersja 1
      print("Time parallel version 1: ")
      printElapsedTime(parallelQuickSort(lists(i)))

      //Quick Sort ze wspolbieznoscia wersja 2
      print("Time parallel version 2: ")
      printElapsedTime(parallelQuickSort2(lists(i)))
      number *= 10

      //Quick Sort bez zrownoleglania z rekursja ogonowa
      print("Time not parallel tail-recursive: ")
      printElapsedTime(quickSortTail(lists(i)))

      //Quick Sort ze wspolbieznoscia wersja 1 z rekursja ogonowa
      print("Time parallel tail-recursive version 1: ")
      printElapsedTime(parallelQuickSortTail(lists(i)))

      //Quick Sort ze wspolbieznoscia wersja 2 z rekursja ogonowa
      print("Time parallel tail-recursive version 2: ")
      printElapsedTime(parallelQuickSortTail2(lists(i)))

    //**********AVERAGE OF BINARY TREE NODES***************************
    println("\n***************AVERAGE OF BINARY TREE NODES***************")

    for(i <- 0 to 4)
      println("\nNUMBER OF NODES: " + (scala.math.pow(2, depths(i)+1)-1) + "\n")

      //Srednia wezlow drzewa binarnego bez zrownoleglania
      print("Time not parallel: ")
      printElapsedTime(treeAverage(trees(i)))

      //Srednia wezlow drzewa binarnego ze zrownoleglaniem
      print("Time parallel: ")
      printElapsedTime(parallelTreeAverage(trees(i)))

    //**********SQUARING ELEMENTS IN LIST***************************
    println("\n***************SQUARING ELEMENTS IN LIST***************")

    number = 10

    for(i <- 0 to 6)
      println("\nNUMBER OF ELEMENTS: " + number+"\n")

      //Podnoszenie elementow listy do kwadratu bez zrownoleglania
      print("Time not parallel: ")
      printElapsedTime(squareList(lists(i)))

      //Podnoszenie elementow listy do kwadratu ze zrownoleglaniem
      print("Time parallel: ")
      printElapsedTime(parallelSquareList(lists(i)))

      number *= 10

    //**********CALCULATING LUCAS NUMBERS***************************
    println("\n***************CALCULATING LUCAS NUMBERS***************")

    for(i <- 0 to 5)
      println("\nNUMBER INDEX: " + lucasNumbers(i)+"\n")

      //Obliczanie liczby Lucasa bez zrownoleglania
      print("Time not parallel: ")
      printElapsedTime(lucasNumber(lucasNumbers(i)))

      //Obliczanie liczby Lucasa bez zrownoleglania z rekursja ogonowa
      print("Time not parallel tail recursive: ")
      printElapsedTime(lucasNumberTail(lucasNumbers(i)))

      //Obliczanie liczby Lucasa ze zrownoleglaniem
      print("Time parallel: ")
      printElapsedTime(parallelLucasNumber(lucasNumbers(i)))

  }
}
