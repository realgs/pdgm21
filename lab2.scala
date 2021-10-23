import scala.annotation.tailrec

class lab2 {
  //task 1
  def splitBySign(xs: List[Int]): (List[Int], List[Int]) = {
    @tailrec
    def splitBySignIn(xs: List[Int], less: List[Int], more: List[Int]): (List[Int], List[Int]) = {
      if xs == Nil then (less, more)
      else if xs.head < 0 then splitBySignIn(xs.tail, xs.head :: less, more)
      else if xs.head > 0 && xs.head % 2 != 0 then splitBySignIn(xs.tail, less, xs.head :: more)
      else splitBySignIn(xs.tail, less, more)
    }

    splitBySignIn(xs, List(), List())
  }

  splitBySign(List(1, 2, -5, -2, 6, 5))

  //task 2
  def listLength[A](xs: List[A]): Int = {
    if xs != null && xs.nonEmpty then {
      1 + listLength(xs.tail)
    }
    else 0
  }
  //task 3
  def concatLists[A](xs: List[A],ys: List[A]): List[A] = {
    (xs,ys) match  {
      case (List(),_) => ys
      case (_,List()) => xs
      case (_,_) => xs.head :: ys.head :: concatLists(xs.tail,ys.tail)
    }
  }
  concatLists(List(1,2,3,4,5),List(6,7,8,9))
}
