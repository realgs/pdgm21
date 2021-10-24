//task 1
def splitBySign (xs: List[Int]): (List[Int], List[Int]) =
  xs match
    case Nil => (Nil, Nil)
    case h :: t if h < 0 =>
      val (xs1, xs2) = splitBySign(t)
      (h :: xs1, xs2)
    case h :: t if h > 0 && h % 2 == 1 =>
      val (xs1, xs2) = splitBySign(t)
      (xs1, h :: xs2)
    case h :: t => splitBySign(t)

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(Nil) == (Nil, Nil)
splitBySign(List(0, 1, 2, 3, -10, 4, 6)) == (List(-10), List(1, 3))
splitBySign(List(0, 2, 4, 6)) == (Nil, Nil)
splitBySign(List(-3, -1, -5)) == (List(-3, -1, -5), Nil)
splitBySign(List(0, 1, 2, 2, 3)) == (Nil, List(1, 3))

//task 2
def lengthOfList[A] (xs: List[A]): Int =
  if xs == Nil then 0 else lengthOfList(xs.tail) + 1

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(Nil) == 0
lengthOfList(List('1', '2', '3')) == 3
lengthOfList(List("dog")) == 1
lengthOfList(List(1, 2, 3, 4, 5)) == 5

def lengthOfListTail[A] (xs: List[A]): Int =
  def lengthOfListIter[A] (xs: List[A],  n: Int): Int =
    if xs == Nil then n
    else lengthOfListIter(xs.tail, n + 1)
  lengthOfListIter(xs, 0)

lengthOfListTail(List(5, 4, 3, 2)) == 4
lengthOfListTail(Nil) == 0
lengthOfListTail(List('1', '2', '3')) == 3
lengthOfListTail(List("dog")) == 1
lengthOfListTail(List(1, 2, 3, 4, 5)) == 5

//task 3
def joinLists[A] (xs: List[A], ys: List[A]): List[A] =
  (xs, ys) match
    case (h1 :: t1, h2 :: t2) => h1 :: h2 :: joinLists(t1, t2)
    case (h1 :: t1, Nil) => xs
    case (Nil, h2 :: t2) => ys
    case _ => Nil

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(Nil, Nil) == Nil
joinLists(Nil, List('a', 'b', 'c')) == List('a', 'b', 'c')
joinLists(List("dog"), Nil) == List("dog")
joinLists(List(1), List(2, 3, 4)) == List(1, 2, 3, 4)
