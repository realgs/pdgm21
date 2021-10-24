//1
def reverseList[A](list: List[A]): List[A] =
  def reverseListHelper[A](acc: List[A], l: List[A]): List[A] =
    if l==Nil then acc
    else reverseListHelper(l.head::acc, l.tail)
  reverseListHelper(Nil, list)

def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  def splitBySignHelper(list: List[Int], l1: List[Int], l2: List[Int]): (List[Int], List[Int]) =
    list match
      case Nil => (reverseList(l1), reverseList(l2))
      case (h::t) if h < 0 => splitBySignHelper(t, h::l1, l2)
      case (h::t) if h > 0 && h%2 != 0 => splitBySignHelper(t, l1, h::l2)
      case (h::t) => splitBySignHelper(t, l1, l2)
  splitBySignHelper(list, Nil, Nil)

splitBySign(List(-3, -2, -1, 1, 3)) == (List(-3, -2, -1), List(1, 3))
splitBySign(List(-5, 7, 2, 3, -9, 4, 1)) == (List(-5, -9), List(7, 3, 1))
splitBySign(List()) == (List(), List())

//2
def lengthOfList[A](list: List[A]): Int =
  if list==Nil then 0
  else 1 + lengthOfList(list.tail)

lengthOfList(List(1, 2, 3)) == 3
lengthOfList(List(1)) == 1
lengthOfList(List()) == 0

//3
def joinLists[A](l1: List[A], l2: List[A]): List[A] =
  (l1, l2) match
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (h1::t1, h2::t2) => h1::h2::joinLists(t1, t2)

joinLists(List(1, 6, 3, 7, 4, 9), List(4, 5, 6, 2, 7, 9)) == List(1, 4, 6, 5, 3, 6, 7, 2, 4, 7, 9, 9)
joinLists(List(3, 6, 1, 3, 4), List(2, 3)) == List(3, 2, 6, 3, 1, 3, 4)
joinLists(List(2, 3), List(3, 6, 1, 3, 4)) == List(2, 3, 3, 6, 1, 3, 4)
joinLists(List(1, 2, 3), List()) == List(1, 2, 3)
joinLists(List(), List()) == List()
