// Maciej Olejnik
import scala.annotation.tailrec

// first task

def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  @tailrec
  def splitBySignIter(list: List[Int], firstResultList: List[Int], secondResultList: List[Int]): (List[Int], List[Int]) =
    if list == Nil then (firstResultList, secondResultList)
    else if (list.head < 0) then splitBySignIter(list.tail, firstResultList ::: List(list.head), secondResultList)
    else if (list.head % 2 == 1) then splitBySignIter(list.tail, firstResultList, secondResultList ::: List(list.head))
    else splitBySignIter(list.tail, firstResultList, secondResultList)
  splitBySignIter(list, List(), List())

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List()) == (List(), List())
splitBySign(List(2, 4, 6, 8, -1)) == (List(-1), List())

//złożoność obliczeniowa O(n^2) :(
//złożoność pamięciowa ???

// second task

def lengthOfList[A](list: List[A]): Int =
  @tailrec
  def lengthOfListIter[A](list: List[A], accum: Int): Int =
    if list == Nil then accum
    else lengthOfListIter(list.tail, accum + 1)
  lengthOfListIter(list, 0)

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List("string")) == 1
lengthOfList(List()) == 0

// złożoność obliczeniowa O(n) złożoność pamięciowa ???

// third task

def joinLists[A] (firstList: List[A], secondList: List[A]): List[A] =
  @tailrec
  def joinListsIter[A] (firstList: List[A], secondList: List[A], index: Int, resultList: List[A]): List[A] =
    if index % 2 == 0 then
      if firstList == Nil then
        if secondList == Nil then
          resultList
        else
          joinListsIter(firstList, secondList.tail, index + 1, resultList ::: List(secondList.head))
      else
        joinListsIter(firstList.tail, secondList, index + 1, resultList ::: List(firstList.head))
    else if secondList == Nil then
      if firstList == Nil then
        resultList
      else
        joinListsIter(firstList.tail, secondList, index + 1, resultList ::: List(firstList.head))
    else
      joinListsIter(firstList, secondList.tail, index + 1, resultList ::: List(secondList.head))
  joinListsIter(firstList, secondList, 0, List())

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(), List()) == List()
joinLists(List(2, 3, 4), List()) == List(2, 3, 4)

// złożoność obliczeniowa O(n^2) złożoność pamięciowa ???


