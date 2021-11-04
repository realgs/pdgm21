import scala.annotation.tailrec

object lista4 {

  val hasSegmentAt = (segment: String) => (startIndex: Int) => (string: String) =>
    val segmentLength = segment.length
    //TODO tutaj pewnie +1 ale kto wie
    if segmentLength > string.length - startIndex then false
    else
      def startsWithRec(stringIndex: Int, segmentIndex: Int): Boolean =
        if segmentIndex >= segmentLength then true
        else segment.charAt(segmentIndex) == string.charAt(stringIndex) && startsWithRec(stringIndex + 1, segmentIndex + 1)

      startsWithRec(startIndex, 0)


  val isSubstring = (substring: String) => (string: String) =>
    val iterationEndIndex = string.length - substring.length
    if iterationEndIndex < 0 then false
    else
      val isSubstringAt = hasSegmentAt(substring)
      def isSubstringRec(index: Int): Boolean =
        if index > iterationEndIndex then false
        else isSubstringAt(0)(string) || isSubstringRec(index + 1)

      isSubstringRec(0)

  //zadanie 2
  def reverseList[A](xs: List[A]): List[A] =
    @tailrec
    def reverseRec(acc: List[A],xs: List[A]): List[A] =
      if xs == Nil then acc
      else reverseRec(xs.head :: acc, xs.tail)

    reverseRec(Nil, xs)

  //TODO bez ogona

  def join3Lists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
    @tailrec
    def join3ListsRec(firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
      (firstList, secondList) match
        case (Nil, Nil) => thirdList
        case (hd1 :: tl1, Nil) => join3ListsRec(tl1, Nil, hd1 :: thirdList)
        case (firstList, hd2 :: tl2) => join3ListsRec(firstList, tl2, hd2 :: thirdList)

    join3ListsRec(reverseList(firstList), reverseList(secondList), thirdList)


  def main(args: Array[String]): Unit =
    val startsWithKot = hasSegmentAt("kot")(0)
    println(startsWithKot("ala ma kota"))
    println(startsWithKot("ola ma ose"))
    println(startsWithKot("kot ma ale"))
    println(startsWithKot("kot"))
    println(startsWithKot(""))
    println(hasSegmentAt("")(0)(""))
    println(hasSegmentAt("")(0)("rgefgsdfg"))

    println("------------")

    //val hasIndex = isSubstring("indexx0168")

    println("------------")

    println(join3Lists(List(1,2,3), List(4,5,6), List(7,8,9)))
    println(join3Lists(Nil, Nil, Nil))
    println(join3Lists(List(1,2,3), Nil, List(7,8,9)))
}
