
def listLength[A](xs: List[A]): Int =
  if (xs != Nil) then
    1 + listLength(xs.tail)
  else 0

listLength(List(1, 3, 2)) == 3
listLength(List("msms", "qwqwqw", "pool", "asdopkasdo")) == 4
listLength(List()) == 0

def joinLists[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match
        case (Nil, Nil) => Nil
        case (Nil, head2 :: tail2) => head2 :: joinLists(list1, tail2)
        case (head1::tail1, Nil) => head1 :: joinLists(tail1, list2)
        case (head1::tail1, head2::tail2) => head1 :: head2 :: joinLists(tail1, tail2)

joinLists(List(), List()) == List()
joinLists(List(5,4,3,2),List(1,2,3,4,5,6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)