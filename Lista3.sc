//Szymon Sawczuk

//Helping functions
def reverseList[A](list:List[A]):List[A] =
  def reverseListIter(list:List[A], reversed:List[A]):List[A] =
    if list == Nil then reversed
    else reverseListIter(list.tail, list.head::reversed)

  reverseListIter(list, Nil)

//Zadanie 1
//def splitBySign(list:List[Int]):(List[Int], List[Int]) =
//  def splitBySignIter(list:List[Int], list1:List[Int], list2:List[Int]):(List[Int], List[Int]) =
//    if list == Nil then (list1, list2)
//    else if list.head < 0 then splitBySignIter(list.tail, list1:::list.head::Nil, list2)
//    else if list.head > 0 && list.head % 2 != 0 then splitBySignIter(list.tail, list1, list2:::list.head::Nil)
//    else splitBySignIter(list.tail, list1, list2)
//
//  splitBySignIter(list, Nil, Nil)

def splitBySign(list:List[Int]):(List[Int], List[Int]) =
  def splitBySignIter(list:List[Int], list1:List[Int], list2:List[Int]):(List[Int], List[Int]) =
    if list == Nil then (reverseList(list1), reverseList(list2))
    else splitBySignIter(list.tail,
      if list.head < 0 then list.head::list1 else list1,
      if list.head > 0 && list.head % 2 != 0 then list.head::list2 else list2)

  splitBySignIter(list, Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13))
splitBySign(List())
splitBySign(List(2, 0 , -3, 3, 5, -2))

//Zadanie 2
def lengthOfList[A](list:List[A]):Int =
  def lengthOfListIter(list:List[A], result:Int):Int =
    if list == Nil then result
    else lengthOfListIter(list.tail, result + 1)

  lengthOfListIter(list, 0)

lengthOfList(List(5, 4, 3, 2))
lengthOfList(List("Ala", "ma", "kota"))
lengthOfList(List())

//Zadanie 3
//def jointLists[A](list1:List[A], list2:List[A]):List[A] =
//  def jointListsIter(list1:List[A], list2:List[A], resultList:List[A]):List[A] =
//    (list1, list2) match
//      case (Nil, Nil) => reverseList(resultList)
//      case (head::tail, Nil) => jointListsIter(tail, Nil, head::resultList)
//      case (Nil, head::tail) => jointListsIter(Nil, tail, head::resultList)
//      case _ => jointListsIter(list1.tail, list2.tail, list2.head::list1.head::resultList)
//
//  jointListsIter(list1, list2, Nil)

def joinLists[A](list1:List[A], list2:List[A]):List[A] =
  def joinListsIter(list1:List[A], list2:List[A], resultList:List[A]):List[A] =
    (list1, list2) match
      case (head1::tail1, head2::tail2) =>  joinListsIter(tail1, tail2, head2::head1::resultList)
      case _ => reverseList(resultList) ::: (if list1 != Nil then list1 else list2)

  joinListsIter(list1, list2, Nil)

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))
joinLists(List(1, 2, 3), List())
joinLists(List(), List())
joinLists(List("Ala", "kota"), List("ma", "!"))
