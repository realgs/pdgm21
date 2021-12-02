import scala.annotation.tailrec
import scala.util.Random

object lab5 extends App
{
  //ZADANIE 1
  val toDigits: (Int => List[Int]) = number =>
    @tailrec
    def toDigitsIter(number: Int, accumulator: List[Int]): List[Int] =
      if number == 0 then accumulator
      else toDigitsIter(number / 16, (number % 16) :: accumulator)
    if number < 0 then toDigitsIter(-number, List(-1))
    else toDigitsIter(number, Nil)

  //ZADANIE 2
  val toDigitsExtended: ((Int, Int) => List[Int]) = (number, base) =>
    @tailrec
    def toDigitsExtendedIter(number: Int, accumulator: List[Int]): List[Int] =
      if number == 0 then accumulator
      else toDigitsExtendedIter(number / base, (number % base) :: accumulator)
    if number < 0 then toDigitsExtendedIter(-number, List(-1))
    else if base < 0 then throw IllegalArgumentException("Base cannot be negative!")
    else toDigitsExtendedIter(number, Nil)

  println(toDigits(31) == List(1, 15))
  println(toDigits(7) == List(7))

  println()

  println(toDigitsExtended(31, 16) == List(1, 15))
  println(toDigitsExtended(13, 10) == List(1, 3))
  println(toDigitsExtended(37, 2) == List(1, 0, 0, 1, 0, 1))
  //OPTYMALIZACJA: REKURSJA OGONOWA (OPTYMALIZACJA PAMIECIOWA), OPERATOR :: DOŁĄCZENIA DO LISTY
  //ZLOZONOSC OBLICZENIOWA: O(N/M + 1), GDZIE N - LICZBA POCZATKOWA, M - PODSTAWA SYSTEMU LICZBOWEGO
  //ZLOZONOSC PAMIECIOWA: O(1)

  //DEFINICJA DRZEWA BINARNEGO
  sealed trait BTree[+A]
  case object Empty extends BTree[Nothing]
  case class Node[+A](value: A, left: BTree[A], right: BTree[A]) extends BTree[A]

  //ZADANIE 3
  val random = Random()

  def generateTree(depth: Int): BTree[Double] =
    def generateTreeIter(depth: Int): BTree[Double] =
      if depth == 1 then Node(random.nextDouble(), Empty, Empty)
      else Node(random.nextDouble(), generateTreeIter(depth - 1), generateTreeIter(depth - 1))
    if depth <= 0 then Empty
    else generateTreeIter(depth)

  println()
  println(generateTree(1))
  println(generateTree(2))
  println(generateTree(5))

  //ZADANIE 4
  def multiplyTree(tree: BTree[Double]): Double =
    def multiplyTreeIter(tree: BTree[Double], result: Double): Double =
      tree match
        case Empty => result
        case Node(value, left, right) => multiplyTreeIter(left, multiplyTreeIter(right, value * result))
    if tree == Empty then throw new IllegalArgumentException("Tree is empty")
    else multiplyTreeIter(tree, 1.0)

  println()
  println(multiplyTree(generateTree(1)))
  println(multiplyTree(generateTree(2)))
  println(multiplyTree(generateTree(4)))
  //OPTYMALIZACJA PAMIECIOWA PRZEZ REKURSJE OGONOWA
}
