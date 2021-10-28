object Lista3
{
  import scala.annotation.tailrec

  /* Zad.1 */

  def splitBySign: (xs:List[Int]) => (List[Int], List[Int]) = xs =>
    def splitBySignHelper: (xs:List[Int], accB0:List[Int], accOdd:List[Int]) => (List[Int], List[Int]) = (xs, accB0, accOdd) =>
      xs match
        case Nil => (accB0, accOdd)
        case xh::xt =>
          if xh < 0 then splitBySignHelper(xt, accB0:::List(xh), accOdd)
          else if xh %2 != 0 then splitBySignHelper(xt, accB0, accOdd:::List(xh))
          else splitBySignHelper(xt, accB0, accOdd)
    splitBySignHelper(xs, Nil, Nil)


  def reverse[A](xs:List[A]):List[A] =
    xs match
      case Nil => Nil
      case xh::xt => reverse(xt):::List(xh)

  def splitBySign2: (xs:List[Int]) => (List[Int], List[Int]) = xs =>
    def splitBySignHelper: (xs:List[Int], accB0:List[Int], accOdd:List[Int]) => (List[Int], List[Int]) = (xs, accB0, accOdd) =>
      xs match
        case Nil => (reverse(accB0), reverse(accOdd))
        case xh::xt =>
          if xh < 0 then splitBySignHelper(xt, xh::accB0, accOdd)
          else if xh %2 != 0 then splitBySignHelper(xt, accB0, xh::accOdd)
          else splitBySignHelper(xt, accB0, accOdd)
    splitBySignHelper(xs, Nil, Nil)


  def splitBySign3: (xs:List[Int]) => (List[Int], List[Int]) = xs =>
    def splitBySignHelper: (xs:List[Int], accB0:List[Int], accOdd:List[Int]) => (List[Int], List[Int]) = (xs, accB0, accOdd) =>
      xs match
        case Nil => (reverse(accB0), reverse(accOdd))
        case xh::xt if xh < 0 => splitBySignHelper(xt, xh::accB0, accOdd)
        case xh::xt if xh %2 != 0 => splitBySignHelper(xt, accB0, xh::accOdd)
        case xh::xt => splitBySignHelper(xt, accB0, accOdd)
    splitBySignHelper(xs, Nil, Nil)


  /* Zad.2 */
   def lengthOfList[A](xs:List[A]):Int =
     @tailrec
     def lengthOfListHelper[A](xs:List[A], acc:Int):Int =
       if xs == Nil then acc
       else lengthOfListHelper(xs.tail, acc+1)
     lengthOfListHelper(xs, 0)


  /* Zad.3 */
  def joinLists[A](xs:List[A], ys:List[A]):List[A] =
    (xs, ys)match
      case (xh::xt, yh::yt) => xh::yh::joinLists(xt, yt)
      case (xh::xt, Nil) => xs
      case (Nil, yh::yt) => ys
      case (Nil, Nil) => Nil


  def main(args: Array[String]): Unit =
  {
    println("splitBySign test:")
    println(splitBySign(List(1, -3, 5, -7, 9, 1, 3, -4)))
    println(splitBySign(List(-3, 8, 5)))
    println(splitBySign(List()))
    println()

    println("splitBySign2 test:")
    println(splitBySign2(List(1, -3, 5, -7, 9, 1, 3, -4)))
    println(splitBySign2(List(-3, 8, 5)))
    println(splitBySign2(List()))
    println()

    println("lengthOfList test:")
    println(lengthOfList(List(1, 3, 5, 7)))
    println(lengthOfList(List("a", "b")))
    println(lengthOfList(List()))
    println()

    println("joinLists test:")
    println(joinLists(List(1, 3, 5, 7), List(2, 4, 6, 8)))
    println(joinLists(List(1, 3, 5, 7), List(2, 4, 6)))
    println(joinLists(List(), List()))

  }
}


