//n^2
def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  def splitBySignIn(number: List[Int], list1: List[Int], list2: List[Int]): (List[Int], List[Int]) =
    if number != Nil then (number.head < 0, number.head % 2 != 0) match
      case (true, _) => splitBySignIn(number.tail, list1 ::: List(number.head), list2)
      case (false, true) => splitBySignIn(number.tail, list1, list2 ::: List(number.head))
      case (_, _) => splitBySignIn(number.tail, list1, list2)
    else (list1, list2)
 splitBySignIn(list, List(), List())

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(-5, -10, -20, 2, 4, 8, 11)) == (List(-5, -10, -20), List(11))
splitBySign(List()) == (List(), List())

//n

def listLength[A](xs: List[A]): Int =
  if (xs != Nil) then
    1 + listLength(xs.tail)
  else 0

listLength(List(1, 3, 2)) == 3
listLength(List("msms", "qwqwqw", "pool", "asdopkasdo")) == 4
listLength(List()) == 0

//n^2

def joinLists[A](list1: List[A], list2: List[A]): List[A] =
  (list1, list2) match
    case (Nil, Nil) => Nil
    case (Nil, head2 :: tail2) => head2 :: joinLists(list1, tail2)
    case (head1 :: tail1, Nil) => head1 :: joinLists(tail1, list2)
    case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: joinLists(tail1, tail2)

joinLists(List(), List()) == List()
joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(2, 1, 3, 4), List(0, 9, 2)) == List(2, 0, 1, 9, 3, 2, 4)

