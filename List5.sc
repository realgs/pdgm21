// Jakub Szwedowicz
import scala.annotation.tailrec

// Zadanie 1
def decimalToHex(number: Int): List[Int] =
  @tailrec
  def convert(numer: Int, listOfHex: List[Int]): List[Int] =
    if (numer == 0) then listOfHex
    else
      val hexDigit = numer % 16
      convert(numer / 16, hexDigit :: listOfHex)

  if(number < 0) then List()
  else
    convert(number, List())

decimalToHex(31)


// Zadanie 2
def decimalToBase(number: Int, base: Int): List[Int] =
  @tailrec
  def convert(numer: Int, listOfNumbersOfBase: List[Int]): List[Int] =
    if (numer == 0) then listOfNumbersOfBase
    else
      val hexDigit = numer % base
      convert(numer / base, hexDigit :: listOfNumbersOfBase)

  if(number < 0 || base <= 1) then List()
  else
    convert(number, List())

decimalToBase(31, 16)


// Zadanie 3
import scala.collection.mutable
import scala.util.Random

sealed trait BTree[+A]
case object Empty extends BTree[Nothing]
case class Node[A](elem: A, left: BTree[A] = Empty, right: BTree[A] = Empty) extends BTree[A]


def generateBinaryTree(depth: Int): BTree[Double] = depth match
  case 0 => Empty
  case _ => Node(Random.nextDouble(), generateBinaryTree(depth - 1), generateBinaryTree(depth - 1))

// Zadanie 4

import annotation.tailrec

def multElemtsInTree(tree: BTree[Double]): Double =
  @tailrec
  def DFS(toVisit: List[BTree[Double]], mult: Double): Double = toVisit match
    case Nil => mult
    case Empty :: t => DFS(t, mult)
    case Node(e, l, r) :: t => DFS(l :: r :: t, mult * e)

  DFS(List(tree), 1)

multElemtsInTree(Node(1, Node(2, Node(3, Empty, Empty), Empty), Node(3, Node(2, Empty, Empty), Empty)))

// Zadanie 5 - Brak
