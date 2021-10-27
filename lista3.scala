import scala.annotation.tailrec

object lista3 {

  def main(args: Array[String]): Unit = {
    println(splitBySign(List(-3,-6,7,-9,13)))
    println(splitBySign(List(-2,-1,0,1,2,3,4,5)))
    println(splitBySign(List(-5,-4,-3)))
    println(splitBySign(List(1,3,5,7)))
    println(splitBySign(List(0,2,4,6,8)))

    println("---------")

    println(lengthOfList(List(5,4,3,2)))
    println(lengthOfList(List("foo")))
    println(lengthOfList(Nil))

    println("----------")

    println(joinLists(List(5,4,3,2), List(1,2,3,4,5,6)))
    println(joinLists(List(1,2,3,4,5), Nil))
    println(joinLists(Nil, List(1,2,3,4,5)))
    println(joinLists(List('a','b','c'), List('x','y','z')))
    println(joinLists(Nil, Nil))
  }

  def reverseList[A](xs: List[A]): List[A] =
    @tailrec
    def reverseRec(acc: List[A],xs: List[A]): List[A] =
      if xs == Nil then acc
      else reverseRec(xs.head :: acc, xs.tail)

    reverseRec(Nil, xs)

  def splitBySign(xs: List[Int]): (List[Int], List[Int]) =
    @tailrec
    def splitRec(xs: List[Int], negList: List[Int], posOddList: List[Int]): (List[Int], List[Int]) =
      xs match
        case Nil => (reverseList(negList), reverseList(posOddList))
        case hd::tl if hd < 0 => splitRec(tl, hd::negList, posOddList)
        case hd::tl if hd % 2 != 0 => splitRec(tl, negList, hd::posOddList)
        case hd::tl => splitRec(tl, negList, posOddList)

    splitRec(xs, Nil, Nil)

  def lengthOfList[A](xs: List[A]) =
    @tailrec
    def lengthRec(acc: Int, xs: List[A]): Int =
      if xs == Nil then acc
      else lengthRec(acc + 1, xs.tail)

    lengthRec(0, xs)

  def joinLists[A](as: List[A], bs: List[A]): List[A] =
    @tailrec
    def joinRec(acc: List[A], switch: Boolean, as: List[A], bs: List[A]): List[A] =
      (as, bs, switch) match
        case (Nil, Nil, _) => reverseList(acc)
        case (hd::tl, Nil, _) => joinRec(hd::acc, false, tl, Nil)
        case (Nil, hd::tl, _) => joinRec(hd::acc, true, Nil, tl)
        case (hd::tl, bs, false) => joinRec(hd::acc, true, tl, bs)
        case (as, hd::tl, true) => joinRec(hd::acc, false, as, tl)

    joinRec(Nil, false, as, bs)

}
