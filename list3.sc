def reverse[A](xs: List[A]): List[A] =
  def reverseTail[A](xs: List[A], result: List[A]): List[A] =
    if xs == Nil then result
    else reverseTail(xs.tail, xs.head::result)
  reverseTail(xs, Nil)


// task 1

def splitBySign(xs: List[Int]): (List[Int], List[Int]) =
  def splitBySignTail(xs: List[Int], ys: List[Int], zs: List[Int]): (List[Int], List[Int]) =
    if xs == Nil then (reverse(ys), reverse(zs))
    else if xs.head < 0 then splitBySignTail(xs.tail,xs.head::ys, zs)
    else if xs.head % 2 == 1 then splitBySignTail(xs.tail,ys,xs.head::zs)
    else splitBySignTail(xs.tail,ys,zs)
  splitBySignTail(xs,Nil,Nil)


splitBySign(List(-3,-6,7,-9,13)) == (List(-3,-6,-9),List(7,13))
splitBySign(List()) == (List(),List())
splitBySign(List(-3,-6,-7,-9,-13)) == (List(-3,-6,-7,-9,-13),List())
splitBySign(List(0,6,7,9,13)) == (List(),List(7,9,13))


// task 2

def lengthOfList[A](xs: List[A]): Int =
  def lengthOfListTail[A](xs: List[A], result: Int): Int =
    if xs == Nil then result
    else lengthOfListTail(xs.tail, 1 + result)
  lengthOfListTail(xs, 0)


lengthOfList(List(5,4,3,2)) == 4
lengthOfList(List()) == 0
lengthOfList(List("aa","a","aaaaa")) == 3


// task 3

def joinLists[A](xs: List[A], ys: List[A]): List[A] =
  def joinListsTail[A](xs: List[A], ys: List[A], result: List[A]): List[A] =
    (xs,ys) match
      case (Nil, Nil) => reverse(result)
      case (Nil, h2::t2) => joinListsTail(xs,t2,h2::result)
      case (h1::t1, Nil) => joinListsTail(t1,ys,h1::result)
      case(h1::t1, h2::t2) => joinListsTail(t1,t2,h2::h1::result)
  joinListsTail(xs,ys,Nil)


joinLists(List(5,4,3,2), List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6)
joinLists(List(), List()) == List()
joinLists(List(), List("a","aa")) == List("a","aa")
joinLists(List(5,4,3,2), List()) == List(5,4,3,2)
