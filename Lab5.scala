package main.paradygmaty

package paradygmaty
import scala.annotation.tailrec
import scala.util.Random

object Main {

  //Zadanie 1

  //Złożoność obliczeniowa O(n/16+1), n - liczba początkowa
  //Złożoność pamięciowa O(1) - rekurencja ogonowa
  // Zadbałem o złozonosc poprzez uzycie rekurencji ogonowej oraz
  // uzycie :: ktore dolacze elemeny na poczatek listy
  def decimalToHex(number: Int): List[Int] =
    @scala.annotation.tailrec
    def decimalToHexHelper(number: Int, resultList: List[Int]): List[Int] =
      if number == 0 then resultList
      else decimalToHexHelper( number/16 , number%16 :: resultList)
    if number > 0 then decimalToHexHelper(number, List())
    else if number == 0 then List(0)
    else -1::decimalToHexHelper(-number, List())


  def decimalToAnySystem(numberAndConvertToSystem: List[Int]): List[Int] =
    @scala.annotation.tailrec
    def decToAnySystemHelper(number: Int, systemNumber: Int, resultList: List[Int]) : List[Int] =
      if number == 0 then resultList
      else decToAnySystemHelper(number/systemNumber, systemNumber, (number%systemNumber)::resultList)

    if numberAndConvertToSystem(1) <= 1 then throw new Exception("Wrong conversion system")
    else
      if numberAndConvertToSystem(0) == 0 then List(0)
      else if numberAndConvertToSystem(0) > 0 then decToAnySystemHelper(numberAndConvertToSystem(0), numberAndConvertToSystem(1), Nil)
      else -1::decToAnySystemHelper(-numberAndConvertToSystem(0), numberAndConvertToSystem(1), Nil)

  //Drzewa

  sealed trait BTree[+A]
  case object Empty extends BTree[Nothing]
  case class Node[+A](value: A, left: BTree[A], right: BTree[A]) extends BTree[A]


  // Zadanie 3
  //stworzenie val random, żeby nie tworzyć cały czas nowej instancji obiektu Random
  val random = Random();
  def generateTree(depth: Int): BTree[Double] =

    def generateTreeHelper(depth: Int): BTree[Double] =
      if depth == 1 then Node(random.nextDouble(), Empty, Empty)
      else Node(random.nextDouble(), generateTreeHelper(depth-1), generateTreeHelper(depth-1))

    if depth <= 0 then Empty
    else generateTreeHelper(depth)

  // Zadanie 4
  def multiplyTree(tree: BTree[Double]): Double =
    def multiplyTreeHelper(tree: BTree[Double], result: Double): Double =
      tree match
        case Empty => result
        case Node(value, left, right) => multiplyTreeHelper(left, multiplyTreeHelper(right, value*result))
    if tree == Empty then throw new IllegalArgumentException("Tree is empty ")
    else multiplyTreeHelper(tree, 1.0)

  //Zadanie 5

  //tworzy drzewo z listy
  def generateTreeFromList[A](list: List[A]): BTree[A] =
    list match
      case Nil => Empty
      case head::tail =>
        val tmp = tail.splitAt(tail.length/2)
        Node(head, generateTreeFromList(tmp._1), generateTreeFromList(tmp._2))


  //funkcja do sprawdzenia czy lista zawiera element
  def listContains[A](list: List[A], element: A): Boolean =
    if list == Nil then false
    else if list.head == element then true
    else listContains(list.tail, element)


  /*
    Przykład użycia tego drzewa:
    -algorytm pobiera znajomych z mojego profilu, którzy spełniają dany predykat np. pracują/studiują w IT,
    jeśli spełnia to dodaje go do drzewa, to samo robi z listą znajomych, którzy zostali dodani,
    (oczywiście tylko do podanej głębokości znajomych, znajomych), następnie drzewo będzie zawierało
    powtórzenia, które należy usunąć.
    Nie zastępujemy ich niczym, zmniejszamy tym samym drzewo do przyszłego przetworzenia

    W przykładach pod tymi dwoma funkcjami mamy specyficznie dla liczb Double, które
    to będą zastępowane przez -1

  */

  def deleteDuplicateDepth[A](tree: BTree[A]): BTree[A] =
    def generateList[A](stack: List[BTree[A]], elements: List[A]): List[A]=
      stack match
        case Nil => Nil
        case Empty::tail => generateList(tail, elements)
        case Node(value, left, right)::tail =>
          if !listContains(elements, value) then value::generateList(left::right::tail, value::elements)
          else generateList(left::right::tail,elements)

    generateTreeFromList(generateList(List(tree),List()))


  def deleteDuplicatesBreadth[A](tree: BTree[A]): BTree[A] =
    def deleteHelper[A](queue: List[BTree[A]], visited: List[A]): List[A]=
      queue match
        case Nil => Nil
        case Empty::tail => deleteHelper(tail, visited)
        case Node(value, left, right)::tail =>
          if listContains(visited, value) then deleteHelper(tail:::List(left, right), visited)
          else value::deleteHelper(tail:::List(left, right), value::visited)
    if tree != Empty then
      generateTreeFromList(deleteHelper(List(tree), List()))
    else Empty

  /*
    Jest to dokładnie ta sama funkcja co deleteDuplicatesBreadth, jednak
    jest specyficznie podane co będzie w niej występowało, więc możemy
    dać coś spoza zakresu, co będzie jako zastępstwo np. w tym drzewie będą tylko dodatnie liczby
    więc -1.0 będzie dobrym zastępstwem
  */

  def deleteDuplicatesBreadthWithReplace(tree: BTree[Double]): BTree[Double] =
    def deleteHelper(queue: List[BTree[Double]], visited: List[Double]): List[Double]=
      queue match
        case Nil => Nil
        case Empty::tail => deleteHelper(tail, visited)
        case Node(value, left, right)::tail =>
          if listContains(visited, value) then -1::deleteHelper(tail:::List(left, right), visited)
          else value::deleteHelper(tail:::List(left, right), value::visited)

    if tree != Empty then
      generateTreeFromList(deleteHelper(List(tree), List()))
    else Empty

  def deleteDuplicateDepthWithReplace(tree: BTree[Double]): BTree[Double] =
    def generateList(stack: List[BTree[Double]], elements: List[Double]): List[Double]=
      stack match
        case Nil => Nil
        case Empty::tail => generateList(tail, elements)
        case Node(value, left, right)::tail =>
          if !listContains(elements, value) then value::generateList(left::right::tail, value::elements)
          else -1::generateList(left::right::tail,elements)

    generateTreeFromList(generateList(List(tree),List()))


  //====================== STARA DEFINICJA ========================================

  // Uwaga, to jest stara definicja gdy polecenie miało treść:
// Napisz funkcję generującą drzewo o głębokości N (liczba poziomów drzewa), zawierającą losowe liczby całkowite. Drzewo pełne.
// Jednak zostawiłem
  def generateTreeOld(depth: Int, maxValue: Int): BTree[Int] =
    val random = new Random();

    //funkcja do generowania liczby losowej z zakresu -n do n BEZ 0!
    def generateRandomNumber(n: Int): Int =
      val result = 1+random.nextInt(n)
      if (random.nextInt(2)==0) result * (-1)
      else result

    def generateTreeHelper(depth: Int): BTree[Int] =
      if depth == 1 then Node(generateRandomNumber(maxValue), Empty, Empty)
      else Node(generateRandomNumber(maxValue), generateTreeHelper(depth-1), generateTreeHelper(depth-1))

    if maxValue <= 0 then
      throw new IllegalArgumentException(" min value can't be less or equal 0")
    else if depth < 0 then
      Empty
    else
      generateTreeHelper(depth)



  //Zadanie 4 - dla starej definicji

  // To również jest funkcja dla starej definicji drzewa
  def multiplyTreeOld(tree: BTree[Int]): BigInt =
    def multiplyTreeHelper(tree: BTree[Int]): BigInt=
      tree match
        case Node(value, left, right) => value * multiplyTreeHelper(left) * multiplyTreeHelper(right)
        case Empty => 1
    tree match
      case node: Node[Int] => multiplyTreeHelper(tree)
      case _ => throw new IllegalArgumentException(" tree is empty ")


  //===================== POWYŻSZE SĄ DO STAREJ DEFINICJI =====================
  //=====================    TE DOBRE SĄ JESZCZE WYŻEJ    =====================

  def main(args: Array[String]): Unit = {


    println()
    println("testy zad 1:")
    println("testy dziesiętne na 16")
    println( decimalToHex(31) == List(1,15))
    println( decimalToHex(-31) == List(-1, 1, 15) )
    println( decimalToHex(0) == List(0) )
    println( decimalToHex(100) == List(6, 4)  )

    println()
    println("Zadanie 2")
    println()
    println("testy na system 16")
    println( decimalToAnySystem(List(31, 16)) == List(1,15))
    println( decimalToAnySystem(List(-31, 16)) == List(-1, 1, 15) )
    println( decimalToAnySystem(List(0, 16)) == List(0) )
    println( decimalToAnySystem(List(100, 16)) == List(6, 4)  )

    println()
    println("testy na system 2")
    println( decimalToAnySystem(List(31, 2))== List(1, 1, 1, 1, 1) )
    println( decimalToAnySystem(List(-31, 2)) == List(-1, 1, 1, 1, 1, 1) )
    println( decimalToAnySystem(List(0, 2)) == List(0) )
    println( decimalToAnySystem(List(100, 2)) == List(1, 1, 0, 0, 1, 0, 0) )

    println()
    println("testy na system 5")
    println( decimalToAnySystem(List(3196, 5)) == List(1, 0, 0, 2, 4, 1))
    println( decimalToAnySystem(List(-3196, 5)) == List(-1, 1, 0, 0, 2, 4, 1) )
    println( decimalToAnySystem(List(0, 5)) == List(0) )
    println( decimalToAnySystem(List(99, 5)) == List(3, 4, 4) )


    println()
    println("Zadanie 3")
    println( generateTree(3))
    println( generateTree(5))
    println( generateTree(1))


    println()
    println("UWAGA, pierwsza czasc odnosi sie do starych")


    println()
    println("Zadanie 3 - stare")
    println("Wyświetlenie przykładowych drzew ze starego zadania")
    println(generateTreeOld(3, 10))
    println(generateTreeOld(2, 100))
    println(generateTreeOld(1, 5))

    println()
    println("Zadanie 4 - stare")

    println("Drzewa do testów:")
    val tree1 = Node(1,Empty,Empty)
    val tree2 = Node(12,Node(10,Empty,Empty),Node(66,Empty,Empty))
    val tree3 = Node(2,Node(8,Node(2,Empty,Empty),Node(3,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(6,Empty,Empty)))
    val tree4 = Node(-2,Node(5,Empty,Empty),Node(5,Empty,Empty))

    println(tree1)
    println(tree2)
    println(tree3)
    println(tree4)

    println()
    println("testy multiply")
    println(multiplyTreeOld(tree1) == 1)
    println(multiplyTreeOld(tree2) == 7920)
    println(multiplyTreeOld(tree3) == 1152)
    println(multiplyTreeOld(tree4) == -50)

    println()
    println("obliczanie iloczynów losowych drzew - stare")
    println(multiplyTreeOld(generateTreeOld(1, 10)))
    println(multiplyTreeOld(generateTreeOld(2, 10)))
    println(multiplyTreeOld(generateTreeOld(3, 100)))
    println(multiplyTreeOld(generateTreeOld(5, 100)))

    println()
    println("obliczanie iloczynów losowych drzew - nowe")
    println(multiplyTree(generateTree(1)))
    println(multiplyTree(generateTree(2)))
    println(multiplyTree(generateTree(3)))
    println(multiplyTree(generateTree(5)))



    println()
    println("Zadanie 5")
    println("Usuwanie duplikatów breadth - drzewa stare:")
    val treeTest = Node(2,Node(8,Node(2,Empty,Empty),Node(3,Empty,Empty)),Node(2,Node(1,Empty,Empty),Node(6,Empty,Empty)))
    println("drzewo: "+ treeTest)
    println("breadth: " + deleteDuplicatesBreadth(treeTest) )
    println("depth: " + deleteDuplicateDepth(treeTest) )

    println()
    println()
    println("kolejne testy")
    val tree = Node(2, Node(8, Node(2, Node(2, Node(5, Empty, Node(7, Empty, Empty)), Node(1, Empty, Empty)),Empty), Node(3,Empty,Empty)), Node(2, Node(1,Empty,Empty), Node(6,Empty,Empty)))

    val depthtree = Node(2,Node(8,Node(5,Empty,Empty),Node(7,Empty,Empty)),Node(1,Node(3,Empty,Empty),Node(6,Empty,Empty)))
    val breathtree = Node(2,Node(8,Node(3,Empty,Empty),Node(1,Empty,Empty)),Node(6,Node(5,Empty,Empty),Node(7,Empty,Empty)))

    println()
    println("testy sprawdzające usuwanie")
    println("depth "+(deleteDuplicateDepth(tree) == depthtree).toString)
    println("breadth" + (deleteDuplicatesBreadth(tree) == breathtree).toString)

    println()
    println()
    println("testy nowe")
    val treeNew = Node(2.0, Node(8.0, Node(2.0, Node(2.0, Node(5.0, Empty, Node(7.0, Empty, Empty)), Node(1.0, Empty, Empty)),Empty), Node(3.0,Empty,Empty)), Node(2.0, Node(1.0,Empty,Empty), Node(6.0,Empty,Empty)))
    println()

    println("drzewo "+ treeNew)
    println("breadth: " + (deleteDuplicatesBreadthWithReplace(treeNew) == Node(2.0,Node(8.0,Node(-1.0,Empty,Node(-1.0,Empty,Empty)),Node(3.0,Empty,Node(1.0,Empty,Empty))),Node(6.0,Node(-1.0,Empty,Node(5.0,Empty,Empty)),Node(-1.0,Empty,Node(7.0,Empty,Empty)))) ))
    println("depth: " + (deleteDuplicateDepthWithReplace(treeNew) == Node(2.0,Node(8.0,Node(-1.0,Empty,Node(-1.0,Empty,Empty)),Node(5.0,Empty,Node(7.0,Empty,Empty))),Node(1.0,Node(3.0,Empty,Node(-1.0,Empty,Empty)),Node(-1.0,Empty,Node(6.0,Empty,Empty))))))



  }

}

