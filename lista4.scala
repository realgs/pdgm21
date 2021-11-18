//Maciej Olejnik 260444
import scala.annotation.tailrec

//First task

def find (queriedList: List[String], queryList: List[String]): List[String] =
  def wordIter (queried: String, query: String, foundWord: String): String =
    if queried == "" || query == "" then
      foundWord
    else if queried.head == query.head then
      wordIter(queried.tail, query.tail, foundWord + query.head)
    else
      wordIter(queried.tail, query, foundWord)

  def findIter (queried: String, queryList: List[String]): String =
    if queryList != Nil && queryList.head != "" then
      if wordIter(queried, queryList.head, "") == queryList.head then
        queried
      else
        findIter(queried, queryList.tail)
    else ""

  if queriedList != Nil then
    val found = findIter(queriedList.head, queryList)
    if found != "" then found :: find(queriedList.tail, queryList)
    else find(queriedList.tail, queryList)
  else
    Nil

find(List("ABCD", "CADS", "BBB", "CCC"), List("A", "B", "AB", ""))

//Second task

// n is the summed length of lists
// Computational complexity = O(n)
// Memory complexity = O(n)

def connectLists[A] (firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
  if firstList != Nil then
    firstList.head :: connectLists(firstList.tail, secondList, thirdList)
  else if secondList != Nil then
    secondList.head :: connectLists(firstList, secondList.tail, thirdList)
  else if thirdList != Nil then
    thirdList.head :: connectLists(firstList, secondList, thirdList.tail)
  else
    Nil

// Computational complexity = O(n)
// Memory complexity = O(1)

def connectListsTail[A] (firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
  @tailrec
  def connectListsIter[A] (firstList: List[A], secondList: List[A], thirdList: List[A], resultList: List[A]): List[A] =
    if firstList != Nil then
      connectListsIter(firstList.tail, secondList, thirdList, resultList ::: firstList.head :: List())
    else if secondList != Nil then
      connectListsIter(firstList, secondList.tail, thirdList, resultList ::: secondList.head :: List())
    else if thirdList != Nil then
      connectListsIter(firstList, secondList, thirdList.tail, resultList ::: thirdList.head :: List())
    else
      resultList

  connectListsIter(firstList, secondList, thirdList, List())

connectLists(List(), List(), List())
connectListsTail(List(), List(), List())
connectLists (List(1, 2), List(3, 4), List(5, 6))
connectListsTail(List(1, 2), List(3, 4), List(5, 6))
