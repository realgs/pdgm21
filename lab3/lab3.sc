import scala.::
//Zadanie 1

def splitBySign(xs : List[Int]) : (List[Int], List[Int]) =
  xs match {
    case Nil => (Nil, Nil)
    case h :: t => if h < 0 then {
      val p = splitBySign(t)
      (h :: p._1, p._2)
    }
    else if h % 2 == 0 then splitBySign(t)
    else {
      val p = splitBySign(t)
      (p._1, h :: p._2)
    }
  }

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List()) == (List(), List())
splitBySign(List(-1, -2, -3)) == (List(-1, -2, -3), List())
splitBySign(List(0, 2, 4)) == (List(), List())
splitBySign(List(-1, 4, 3, -6, 2, 7)) == (List(-1, -6), List(3, 7))
splitBySign(List(1, 3, 5)) == (List(), List(1, 3, 5))



//Zadanie 2
def lengthOfList[A](xs : List[A]) : Int = {
  xs match {
    case Nil => 0
    case _ => 1 + lengthOfList(xs.tail)
  }
}
lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List(2)) == 1
lengthOfList(Nil) == 0



//Zadanie 3

def joinLists[A](xs : List[A], ys : List[A]) : List[A] =
  (xs, ys) match {
    case (Nil, Nil) => Nil
    case (_, Nil) => xs
    case (Nil, _) => ys
    case (_, _) => xs.head :: ys.head :: joinLists(xs.tail, ys.tail)
  }

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(Nil, List(2, 5, 3)) == List(2, 5, 3)
joinLists(List(9, 8, 7), Nil) == List(9, 8, 7)
joinLists(Nil, Nil) == List()
