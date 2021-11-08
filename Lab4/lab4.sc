import scala.annotation.tailrec


def find(list: List[String], frases: String): List[String] ={

}





def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  (first, second) match
    case (h1::t1, l2) => h1 :: joinLists(t1, l2, third)
    case (Nil, h2::t2) => h2 :: joinLists(Nil, t2, third)
    case (Nil, Nil) => third
}

def reverseList[A](list: List[A]): List[A] ={
  @tailrec
  def innerReverse[A](list: List[A], reversedList: List[A]): List[A] ={
    list match
      case h1:: t1 => innerReverse(t1, h1 :: reversedList)
      case Nil => reversedList
  }
  innerReverse(list, List())
}

def joinListsTail[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  @tailrec
  def iter[A](first: List[A], second: List[A], third: List[A], finalList: List[A]): List[A] ={
    (first, second, third) match
      case (h1::t1, l2, l3) => iter(t1, l2, l3, h1:: finalList)
      case (Nil, h2::t2, l3) => iter(Nil, t2, l3, h2 :: finalList)
      case (Nil, Nil, h3::t3) => iter(Nil, Nil, t3, h3 :: finalList)
      case(Nil, Nil, Nil) => finalList
  }
  reverseList(iter(first, second, third, List()))
}



val a = List(1,2,3,4,5,6)
val b = List(7,8,9)
val c = List(10,11,12)
joinLists(a,b,c)
joinListsTail(a,b,c)