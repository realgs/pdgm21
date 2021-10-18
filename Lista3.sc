
//Zadanie 1

def reverseList [A] (list: List[A]): List[A] =
  def reverseHelper(list: List[A], resultList: List[A]): List[A]=
    if list==Nil then resultList
    else reverseHelper(list.tail, list.head::resultList)
  reverseHelper(list, List())

def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  def splitHelper(list: List[Int], negative: List[Int], positiveOdd: List[Int]): (List[Int], List[Int]) =
    if (list != Nil) then
      (list.head<0, list.head%2 != 0) match
        case(false, true) => splitHelper(list.tail, negative, list.head::positiveOdd)
        case(true, true) => splitHelper(list.tail, list.head::negative, positiveOdd)
        case(_, false) => splitHelper(list.tail, negative, positiveOdd)
    else (reverseList(negative), reverseList(positiveOdd))
  splitHelper(list, List(), List())

splitBySign(List(1, 2, -4, -5, 5, 0, 2, 10, 15, 22, -123, 17, -69, 69, 13)) == (List(-5, -123, -69),List(1, 5, 15, 17, 69, 13))
splitBySign(List(-3,-6,7,-9,13)) == (List(-3,-9), List(7, 13))
splitBySign(List()) == (List(), List())
splitBySign(List(-12, -13, -15, 0, 0, 0, 1, 2, 3, 4, 68, 77, 159)) == (List(-13, -15),List(1, 3, 77, 159))


//Zadanie 2
def lengthOfList [A](list: List[A]): Int =
  def lengthHelper [A](list: List[A], listLength: Int): Int =
    if list == Nil then listLength
    else lengthHelper(list.tail, listLength+1)
  lengthHelper(list, 0)

lengthOfList(List(5,4,3,2)) == 4
lengthOfList(List()) == 0
lengthOfList(List("a", "b", "c", "d", "e")) == 5

//Zadanie 3

def joinLists[A] (list1: List[A], list2: List[A]): List[A]=
  if(list1!=Nil && list2!=Nil) then list1.head::list2.head::joinLists(list1.tail, list2.tail)
  else if (list1!=Nil) then list1
  else list2

joinLists(List(1,2,3,4,5,6), List(-1,-2,-3,-4,-5,-6))==List(1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, -6)
joinLists(List(), List()) == List()
joinLists(List(), List(1, 2, 3)) == List(1, 2, 3)
joinLists(List(1,2,3), List()) == List(1, 2, 3)


