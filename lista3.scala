import scala.annotation.tailrec

def reverseList [A](list: List[A]): List[A] =
  @tailrec
  def reverseListIter [A](list: List[A], listReverse: List[A]): List[A] =
    if list == Nil then listReverse
    else reverseListIter(list.tail, list.head :: listReverse)
  reverseListIter(list, Nil)

//zadanie 1

def splitBySign (intList: List[Int]): (List[Int], List[Int]) =
  @tailrec
  def splitBySignIter(intList: List[Int], negativeValues: List[Int], positiveOddValues: List[Int]): (List[Int], List[Int]) =
    intList match
      case (Nil) => (negativeValues, positiveOddValues)
      case (head::tail) if head < 0 => splitBySignIter(tail, head :: negativeValues, positiveOddValues)
      case (head::tail) if head % 2 == 1 => splitBySignIter(tail, negativeValues, head :: positiveOddValues)
      case (_) => splitBySignIter(intList.tail, negativeValues, positiveOddValues)

  splitBySignIter(reverseList(intList), Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(Nil) == (Nil, Nil)
splitBySign(List(2, 4, 6)) == (Nil, Nil)
splitBySign(List(-1, 2, -6, 4)) == (List(-1, -6), Nil)
splitBySign(List(2, 3, 5, 10)) == (Nil, List(3, 5))

//zadanie 2

def lengthOfList [A](list: List[A]): Int =
  @tailrec
  def lengthOfListIter [A](list: List[A], length: Int): Int =
    if list == Nil then lenght else lengthOfListIter(list.tail, length + 1)
  lengthOfListIter(list, 0)

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(Nil) == 0
lengthOfList(List("ala", "ma", "kota")) == 3
lengthOfList(List('x')) == 1

//zadanie 3

def joinLists [A](firstList: List[A], secondList: List[A]): List[A] =
  @tailrec
  def joinListsIter [A](firstList: List[A], secondList: List[A], finalList: List[A]): List[A] =
    (firstList, secondList) match
      case(head1::tail1, head2::tail2) => joinListsIter(tail1, tail2, head2 :: head1 :: finalList)
      case(Nil, head::tail) => joinListsIter(Nil, tail, head::finalList)
      case(head::tail, Nil) => joinListsIter(tail, Nil, head::finalList)
      case(Nil, Nil) => finalList
  reverseList(joinListsIter(firstList, secondList, Nil))

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(Nil, Nil) == Nil
joinLists(Nil, List('x')) == List('x')
joinLists(List('x'), Nil) == List('x')
joinLists(List("Adam", "Bartek"), List("Ania", "Beata")) == List("Adam", "Ania", "Bartek", "Beata")
