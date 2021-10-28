import scala.annotation.tailrec

def reverse [A](xs: List[A]): List[A] =
  @tailrec
  def rev (xs:List[A], ys:List[A]): List[A] =
    if xs == Nil then ys else rev (xs.tail, xs.head::ys)
  rev(xs, Nil)

// zad 1
def splitBySign (xs:List[Int]): (List[Int], List[Int]) =
  @tailrec
  def splitIter (xs:List[Int], neg:List[Int], pos:List[Int]): (List[Int], List[Int]) =
    xs match
      case Nil => (neg, pos)
      case h::t => if h < 0 then splitIter(t, h::neg, pos) else
        if h > 0 && h % 2 == 1 then splitIter(t, neg, h::pos) else
          splitIter(t, neg, pos)
  splitIter(reverse(xs), Nil, Nil)

// testy do 1
splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(Nil) == (Nil, Nil)
splitBySign((List(0, -1, 1, 1, -1, 0, 2, -2, -2, 2, 0))) == (List(-1, -1, -2, -2), List(1, 1))

// zad 2
def lengthOfList [A](xs:List[A]): Int =
  @tailrec
  def len(xs:List[A], n:Int): Int =
    if xs == Nil then n else len(xs.tail, n+1)
  len(xs, 0)

// testy do 2
lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(Nil) == 0
lengthOfList(List("Koperek")) == 1;;

// zad 3
def joinLists [A](xs:List[A], ys:List[A]): List[A] =
  @tailrec
  def join (xs:List[A], ys:List[A], zs:List[A], np:Boolean): List[A] =
    (xs, ys, np) match
      case (Nil, Nil, _) => reverse(zs)
      case (h1::t1, Nil, _) => join(t1, ys, h1::zs, false)
      case (h1::t1, h2::t2, true) => join(t1, ys, h1::zs, false)
      case (Nil, h2::t2, _) => join(xs, t2, h2::zs, true)
      case (h1::t1, h2::t2, false) => join(xs, t2, h2::zs, true)
  join(xs, ys, Nil, true)

// testy do 3
joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(3.2, 5.6, 1.1), Nil) == List(3.2, 5.6, 1.1)
joinLists(Nil, List("Pomysłowe słowo 1", "Pomysłowe słowo 2", "Pomysłowe słowo 3")) == List("Pomysłowe słowo 1", "Pomysłowe słowo 2", "Pomysłowe słowo 3")
joinLists(Nil, Nil) == Nil
