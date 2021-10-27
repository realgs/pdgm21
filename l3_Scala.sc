//Zadanie 1
def splitBySign(xs: List[Int]): (List[Int], List[Int]) =
  def splitBySignIter(xs: List[Int], minus: List[Int], plus: List[Int]): (List[Int], List[Int]) =
    if xs == Nil then (minus, plus)
    else if xs.head < 0 then splitBySignIter(xs.tail, minus:::List(xs.head), plus)
    else if xs.head % 2 == 0 then splitBySignIter(xs.tail, minus, plus)
    else splitBySignIter(xs.tail, minus, plus:::List(xs.head))
  splitBySignIter(xs, List(), List())

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(0, -7, 11, -15, 4, -5, -12, 3, 2, 4)) == (List(-7, -15, -5, -12), List(11, 3))
splitBySign(List(2, 4, 6, 8, 10, 11, -11)) == (List(-11), List(11))
splitBySign(List()) == (List(), List())

//Złożoność obliczeniowa O(n^2)
//Złożoność pamięciowa O(n)

//Zadanie 2
def lengthOfList[A](xs: List[A]): Int =
  def lengthOfListIter[A](xs: List[A], a: Int): Int =
    if xs == Nil then a
    else lengthOfListIter(xs.tail, a+1)
  lengthOfListIter(xs, 0)

lengthOfList(List(0, 1, 2, -5, 4, 61, 3)) == 7
lengthOfList(List('A', 'B', 'C', 'D')) == 4
lengthOfList(List()) == 0

//Złożoność obliczeniowa O(n)
//Złożoność pamięciowa O(1)

//Zadanie 3
def joinLists[A](xs: List[A], ys: List[A]): List[A] =
  (xs, ys) match
    case (hx::tx, hy::ty) => hx::hy::joinLists(tx, ty)
    case (Nil, _) => ys
    case (_, Nil) => xs

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List("x", "y", "z"), List("a", "b", "c", "d")) == List("x", "a", "y", "b", "z", "c", "d")
joinLists(List(), List(-2.0, 3.0)) == List(-2.0, 3.0)
joinLists(List(), List()) == List()

//Złożoność obliczeniowa O(n)
//Złożoność pamięciowa O(n)
