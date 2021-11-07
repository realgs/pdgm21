


def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  first ::: second ::: third
}

val a = List(1,2,3,4,5,6)
val b = List(7,8,9)
val c = List(10,11,12)
joinLists(a,b,c)