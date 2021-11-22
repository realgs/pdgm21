import scala.annotation.tailrec

object Lista4Balicki {

  //Reverse
  def reverse[A](list: List[A]) =
    @tailrec
    def reverseIter[A](reversedList: List[A], list: List[A]): List[A] =
      list match
        case Nil => reversedList
        case h :: t => reverseIter(h :: reversedList, t)

    reverseIter(List(), list)

  def searchElement(elem: String, pattern: String): Boolean =
    (elem, pattern) match
      case (_, "") => true
      case ("", _) => false
      case (elem, patt) =>
        if elem.head == patt.head then searchElement(elem.tail, patt.tail)
        else searchElement(elem.tail, patt)

  //Zadanie 1 rekurencyjnie
  def searchList(inputList: List[String], pattern: String): List[String] =
    inputList match
      case List() => List()
      case head::tail =>
        if searchElement(head, pattern) then head :: searchList(tail, pattern)
        else searchList(tail, pattern)

  def searchNList(inputList: List[String], patternList: List[String]): List[String] =
    def searchN(input: List[String], pattern: List[String]): List[String] =
      (input, pattern) match
        case (List(),_) => List()
        case (elem :: tail, List()) => searchN(tail, patternList)
        case (elem::inputTail, pat::patTail) =>
          if searchElement(elem, pat) then elem :: searchN(inputTail, patternList)
          else searchN(input, patTail)
    searchN(inputList, patternList)

  //Zadanie 1 z rekursją ogonową
  def searchListTail(inputList: List[String], pattern: String): List[String] =
    @tailrec
    def searchListIter(inputList: List[String], acc: List[String]): List[String] =
      inputList match
        case List() => acc
        case head::tail =>
          if searchElement(head, pattern) then searchListIter(tail, head :: acc)
          else searchListIter(tail, acc)
    reverse(searchListIter(inputList, List()))

  def searchNListTail(inputList: List[String], patternList: List[String]): List[String] =
    @tailrec
    def searchNListIter(input: List[String], pattern: List[String], acc: List[String]): List[String] =
      (input, pattern) match
        case (List(),_) => acc
        case (elem :: tail, List()) => searchNListIter(tail, patternList, acc)
        case (elem::inputTail, pat::patTail) =>
          if searchElement(elem, pat) then searchNListIter(inputTail, patternList, elem::acc)
          else searchNListIter(input, patTail, acc)
    reverse(searchNListIter(inputList, patternList, List()))

  //Zadanie 2 rekurencyjnie
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (h :: t, list2, list3) => h :: joinLists(t, list2, list3)
      case (Nil, h :: t, list3) => h :: joinLists(Nil, t, list3)
      case (Nil, Nil, h :: t) => h :: joinLists(Nil, Nil, t)
      case (Nil, Nil, Nil) => Nil

  //Zadanie 2 z rekurencja ogonowa
  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def joinListsIter[A](list1: List[A], list2: List[A], resultList: List[A]): List[A] =
      (list1, list2) match
        case (list1, h :: t) => joinListsIter(list1, t, h :: resultList)
        case (h :: t, Nil) => joinListsIter(t, Nil, h :: resultList)
        case (Nil, Nil) => resultList

    joinListsIter(reverse(list1), reverse(list2), list3)

  def main(args: Array[String]): Unit = {

    println(searchList(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224", "Damian"), "index0168"))
    println(searchListTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224", "Damian"), "index0168"))
    println(searchNList(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224", "Damian"), List("index0168", "Damian")))
    println(searchNListTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224", "Damian"), List("index0168", "Damian")))

    println(reverse(List(1, 2, 3)))
    println(joinListsTail(List(1, 2, 3), List(4, 5, 6, 7), List(8, 9, 10, 11)))
    println(joinLists(List(1, 2, 3), List(4, 5, 6, 7), List(8, 9, 10, 11)))
  }
}

