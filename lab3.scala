//Piotr Zięba

def reverse[A](xs: List[A], res: List[A]): List[A] =
  xs match
    case Nil => res
    case h::t => reverse(t, h::res)

//Zadanie 1
def splitBySign(xs: List[Int]): (List[Int], List[Int]) =
  def splitBySignI(xs: List[Int], negxs: List[Int], posxs: List[Int]): (List[Int], List[Int]) =
    xs match
      case Nil => (negxs, posxs)
      case h::t =>
        if h < 0 then splitBySignI(t, h::negxs, posxs)
        else if h > 0 && h%2 != 0 then splitBySignI(t, negxs, h::posxs)
        else splitBySignI(t, negxs, posxs)
  splitBySignI(reverse(xs, Nil), Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(1, 2, 3, 0)) == (List(),List(1, 3))
splitBySign(List(-5, -7, -2)) == (List(-5, -7, -2),List())
splitBySign(Nil) == (List(),List())

//Zadanie 2
def lengthOfList[A](xs: List[A]): Int =
  def lengthOfListI[A](xs: List[A], counter: Int): Int =
    xs match
      case Nil => counter
      case h::t => lengthOfListI(t, counter + 1)
  lengthOfListI(xs, 0)

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List(-3, -6, 7, -9, 13)) == 5
lengthOfList(List('o', 'l', 'a', 'm', 'a', 'k', 'o', 't', 'a')) == 9
lengthOfList(Nil) == 0

//Zadanie 3
def joinLists[A](xs1: List[A], xs2: List[A]): List[A] =
  def joinListsI[A](xs1: List[A], xs2: List[A], res: List[A]): List[A] =
    (xs1, xs2) match
      case (Nil, h2::t2) => joinListsI(xs1, t2, h2::res)
      case (h1::t1, Nil) => joinListsI(t1, xs2, h1::res)
      case (h1::t1, h2::t2) => joinListsI(t1, t2, h2::h1::res)
      case (Nil, Nil) => res
  reverse(joinListsI(xs1, xs2, Nil), Nil)

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1 , 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(1, 3, 5, 7), List(2, 4, 6, 8, 10, 12)) == List(1, 2, 3, 4, 5, 6, 7, 8, 10, 12)
joinLists(Nil, List(1, 3, 5, 7)) == List(1, 3, 5, 7)
joinLists(Nil, Nil) == List()
