import scala.annotation.tailrec

object l4 {
  def main(args: Array[String]): Unit = {}

  // 1
  def subStr(originalSearched: String, originalWwanted: String): Boolean =
    def subStrHelper(searched: String, wanted: String, nextToBeChecked: String): Boolean =
      (searched, wanted) match
        case (_, "") => true
        case ("", _) => false
        case (searched, wanted) => if searched.head == wanted.head then subStrHelper(searched.tail, wanted.tail, (if nextToBeChecked != "" then nextToBeChecked else searched.tail))
                                   else if nextToBeChecked != "" then subStrHelper(nextToBeChecked, originalWwanted, "")
                                   else subStrHelper(searched.tail, wanted, "")
    subStrHelper(originalSearched, originalWwanted, "")

  def reverseList[A](list: List[A]): List[A] =
    def reverseListHelper[A](acc: List[A], l: List[A]): List[A] =
      if l==Nil then acc
      else reverseListHelper(l.head::acc, l.tail)
    reverseListHelper(Nil, list)

  def find(indexes: List[String], originalMatchers: List[String]): List[String] =
    def findHelper(indexes: List[String], matchers: List[String]): List[String] =
      (indexes, matchers) match
        case (indexesHead :: indexesTail, matchersHead :: matchersTail) if subStr(indexesHead, matchersHead) => indexesHead :: findHelper(indexesTail, originalMatchers)
        case (indexesHead :: indexesTail, matchersHead :: matchersTail) => if matchersTail == Nil then findHelper(indexesTail, originalMatchers) else findHelper(indexes, matchersTail)
        case _ => Nil
    findHelper(indexes, originalMatchers)

  def findT(indexes: List[String], originalMatchers: List[String]): List[String] =
    @tailrec
    def findTHelper(indexes: List[String], matchers: List[String], result: List[String]): List[String] =
      (indexes, matchers) match
        case (indexesHead :: indexesTail, matchersHead :: matchersTail) if subStr(indexesHead, matchersHead) => findTHelper(indexesTail, originalMatchers, indexesHead :: result)
        case (indexesHead :: indexesTail, matchersHead :: matchersTail) => if matchersTail == Nil then findTHelper(indexesTail, originalMatchers, result) else findTHelper(indexes, matchersTail, result)
        case _ => reverseList(result)
    findTHelper(indexes, originalMatchers, Nil)
}
