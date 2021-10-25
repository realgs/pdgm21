//Szymon Bak

//zadanie 1
def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  def splitBySignHelper(list: List[Int], listNegative: List[Int], listPositive: List[Int]): (List[Int], List[Int]) =
    if list == Nil then (listNegative, listPositive)
    else if list.head < 0 then splitBySignHelper(list.tail, listNegative ::: List(list.head), listPositive)
    else if list.head % 2 != 0 then splitBySignHelper(list.tail, listNegative , listPositive ::: List(list.head))
    else splitBySignHelper(list.tail, listNegative, listPositive)
  splitBySignHelper(list, Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(1, 1, 1, 3, 5)) == (List(), List(1, 1, 1, 3, 5))
splitBySign(List()) == (List(), List())

//zadanie 2
def lengthOfList[A](list: List[A]): Int =
  if list == Nil then 0
  else 1 + lengthOfList(list.tail)

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List()) == 0
lengthOfList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) == 10

//zadanie 3
def joinLists[A](list1: List[A], list2: List[A]): List[A] =
  (list1, list2) match
    case (Nil, Nil) => Nil
    case (_, Nil) => list1
    case (Nil, _) => list2
    case (h1::t1, h2::t2) => h1 :: h2 :: joinLists(t1, t2)

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5,1,4,2,3,3,2,4,5,6)
joinLists(List(1, 2, 3, 4), List(-1, -2, -3, -4)) == List(1, -1, 2, -2, 3, -3, 4, -4)
joinLists(List('a', 'b', 'c'), List()) == List('a', 'b', 'c')
joinLists(List(), List('a', 'b', 'c')) == List('a', 'b', 'c')
joinLists(List(), List()) == List()