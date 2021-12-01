import scala.annotation.tailrec
import scala.util.Random

object lab5 extends App
{
  //ZADANIE 1 ORAZ 2
  val toDigits: ((Int, Int) => List[Int]) = (number, base) =>
    @tailrec
    def toDigitsIter(number: Int, accumulator: List[Int]): List[Int] =
      if number == 0 then accumulator
      else toDigitsIter(number / base, (number % base) :: accumulator)
    toDigitsIter(number, Nil)

  println(toDigits(31, 16) == List(1, 15))
  println(toDigits(13, 10) == List(1, 3))
  println(toDigits(37, 2) == List(1, 0, 0, 1, 0, 1))
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
