//1
def reverse (xs: List[Int], result: List[Int]) : (List[Int]) =
  xs match
    case Nil => result
    case (hd :: tl) => reverse(tl, hd :: result)

def splitBySign (xs: List[Int]): (List[Int], List[Int]) =
  def split(xs: List[Int], neg: List[Int], pos: List[Int]) : (List[Int], List[Int]) =
    xs match
      case Nil => (reverse(neg, Nil), reverse(pos, Nil))
      case (hd :: tl) if hd < 0 => split(tl, hd :: neg, pos)
      case (hd :: tl) if hd % 2 != 0 => split(tl, neg, hd :: pos)
      case (hd :: tl) => split(tl, neg, pos)
  split(xs, Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3,-6,-9),List(7, 13))
splitBySign(List()) == (List(), List())
splitBySign(List(0,2,4,6,8)) == (List(), List())

//2
def lengthOfList[A](xs: List[A]): Int =
  if xs == Nil then 0
  else 1+lengthOfList(xs.tail)

lengthOfList(List(1, 2, 3, 4)) == 4
lengthOfList(Nil) == 0
lengthOfList(List(List(List()), List(List()), List(List()))) == 3

//3
def joinLists[A](xs: List[A],ys: List[A]): List[A] =
  (xs, ys) match
    case (Nil, _) => ys
    case ( _, Nil) => xs
    case (hx::tx, hy::ty) => hx :: hy :: joinLists(tx, ty)

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2 ,4, 5, 6)
joinLists(List(), List()) == List()
joinLists(List(), List('a', 'b', 'c')) == List('a', 'b', 'c')
joinLists(List(1, 1, 1, 1, 1, 1, 1), List(2, 2, 2, 2, 2, 2)) == List(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1)
