
def listLength[A](xs: List[A]): Int =
  if (xs != Nil) then
    1 + listLength(xs.tail)
  else 0

listLength(List(1, 3, 2)) == 3
listLength(List("msms", "qwqwqw", "pool", "asdopkasdo")) == 4
listLength(List()) == 0

def joinLists[A](list1: List[A], list2: List[A]): List[A] =
  if list1 != Nil && list2 != Nil  then list1.head :: list2.head ::  joinLists(list1.tail, list2.tail)
  else if (list1 == Nil) then list2
  else list1

joinLists(List(), List()) == List()
joinLists(List(5,4,3,2),List(1,2,3,4,5,6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)