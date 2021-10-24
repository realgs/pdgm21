//Jakub Kowalczyk

import scala.annotation.tailrec

object lab3 {

  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def go(list: List[A], output: List[A]): List[A] =
      if list == Nil then output
      else go(list.tail, list.head::output)
    go(list, Nil)

  //Zadanie 1 - O(n^2)

  def splitBySign(list: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def go1(list: List[Int], negativeNumbers: List[Int], positiveOddNumbers: List[Int]): (List[Int], List[Int]) =
      if list == Nil then (negativeNumbers.reverse, positiveOddNumbers.reverse)
      else if list.head < 0 then go1(list.tail, list.head::negativeNumbers, positiveOddNumbers)
      else if (list.head > 0 && list.head % 2 == 1) then go1(list.tail, negativeNumbers, list.head::positiveOddNumbers)
      else go1(list.tail, negativeNumbers, positiveOddNumbers)
    go1(list, Nil, Nil)
  }

  //Zadanie 2 - O(n)

  def lengthOfList[A](list: List[A]): Int =
    @tailrec
    def go2[A](list: List[A], a: Int): Int =
      if list == Nil then a
      else go2(list.tail, a + 1)
    go2(list, 0)

  //Zadanie 3 - O(n^2)

  def joinLists(listA: List[Int], listB: List[Int]): List[Int] =
    @tailrec
    def go3(listA: List[Int], listB: List[Int], output: List[Int], bool: Boolean): List[Int] =
      if (listA == Nil && listB == Nil) then output.reverse
      else if listA == Nil then go3(listA, listB.tail, listB.head::output, true)
      else if listB == Nil then go3(listA.tail, listB, listA.head::output, true)
      else if bool == true then go3(listA.tail, listB, listA.head::output, false)
      else go3(listA, listB.tail, listB.head::output, true)
    go3(listA, listB, Nil, true)


  def main(args: Array[String]): Unit = {

    println(lengthOfList(List(5,4,3,2)) == 4)
    println(lengthOfList(Nil) == 0)
    println(splitBySign(List(-3,-6,7,-9,13)) == (List(-3, -6, -9), List(7, 13)))
    println(splitBySign(List(0,2,4,6,8,10)) == (Nil, Nil))
    println(joinLists(List(5,4,3,2), List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6))
    println(joinLists(List(1), List(2,3,4,5,6,7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    println(joinLists(List(1,3,4,5,6,7,8,9), List(2)) == List(1,2,3,4,5,6,7,8,9))
    println(joinLists(Nil, Nil) == Nil)

  }

}
