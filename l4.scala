import scala.annotation.tailrec

object l4 {
  def main(args: Array[String]): Unit = {}

  // 1
  def subStr(originalSearched: String, originalWanted: String): Boolean =
    def subStrHelper(searched: String, wanted: String, nextToBeChecked: String): Boolean =
      (searched, wanted) match
        case (_, "") => true
        case ("", _) => false
        case (searched, wanted) => if searched.head == wanted.head then subStrHelper(searched.tail, wanted.tail, (if nextToBeChecked != "" then nextToBeChecked else searched.tail))
                                   else if nextToBeChecked != "" then subStrHelper(nextToBeChecked, originalWanted, "")
                                   else subStrHelper(searched.tail, wanted, "")
    subStrHelper(originalSearched, originalWanted, "")

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

  //2
  def joinLists[A] (list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (h1::t1, _, _) => h1::joinLists(t1, list2, list3)
      case (_, h2::t2, _) => h2::joinLists(Nil, t2, list3)
      case (_, _, h3::t3) => h3::joinLists(Nil, Nil, t3)

  def joinListsT[A] (list1: List[A], list2: List[A], list3: List[A]) =
    def joinListsTHelper[A] (list1: List[A], list2: List[A], list3: List[A], joined: List[A]): List[A] =
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => reverseList(joined)
        case (h1::t1, _, _) => joinListsTHelper(t1, list2, list3, h1::joined)
        case (_, h2::t2, _) => joinListsTHelper(Nil, t2, list3, h2::joined)
        case (_, _, h3::t3) => joinListsTHelper(Nil, Nil, t3, h3::joined)
    joinListsTHelper(list1, list2, list3, Nil)
}
