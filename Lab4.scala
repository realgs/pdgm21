import scala.annotation.tailrec

object Lab4
{
  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def reverseTail[A](list: List[A], reversed: List[A]): List[A] =
      list match
        case Nil => reversed
        case h :: t => reverseTail(t, h :: reversed)
    reverseTail(list, Nil)

  def containsString(stringToCheck: String, stringToFind: String): Boolean =
    if stringToFind == "" then false else
      @tailrec
      def contains(check: String, find: String, tryAgain: String): Boolean =
        if check.length < find.length then false
        else if find == "" then true
        else if check == "" then false
        else if check.head == find.head then contains(check.tail, find.tail, tryAgain)
        else contains(tryAgain, stringToFind, tryAgain.tail)
      contains(stringToCheck, stringToFind, stringToCheck.tail)

  @tailrec
  def containsAnyPattern(listElem: String, patterns: List[String]): Boolean =
    patterns match
      case Nil => false
      case h :: t => if containsString(listElem, h) then true else containsAnyPattern(listElem, t)

  def find(list: List[String], stringToFind: String): List[String] =
    list match
      case Nil => Nil
      case h :: t => if containsString(h,stringToFind) then h :: find(list.tail, stringToFind) else find(list.tail,stringToFind)

  def findTail(list: List[String], stringToFind: String): List[String] =
    @tailrec
    def findHelp(list: List[String], stringToFind: String, result: List[String]): List[String] =
      list match
        case Nil => result
        case h :: t => if containsString(h, stringToFind) then findHelp(list.tail, stringToFind, h :: result) else findHelp(list.tail, stringToFind, result)
    reverse(findHelp(list, stringToFind, Nil))

  def findList(list: List[String], listPhrasesToFind: List[String]): List[String] =
    list match
      case Nil => Nil
      case h :: t => if containsAnyPattern(h, listPhrasesToFind) then h :: findList(t, listPhrasesToFind) else findList(t, listPhrasesToFind)

  def findListTail(list: List[String], listPhrasesToFind: List[String]): List[String] =
    @tailrec
    def findHelp(list: List[String], listPhrasesToFind: List[String], result: List[String]): List[String] =
      list match
        case Nil => result
        case h :: t => if containsAnyPattern(h, listPhrasesToFind) then findHelp(t, listPhrasesToFind, h :: result) else findHelp(t, listPhrasesToFind, result)
    reverse(findHelp(list, listPhrasesToFind, Nil))

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2) match
      case (h :: t, list2) => h :: joinLists(t, list2, list3)
      case (Nil, h :: t) => h :: joinLists(Nil, t, list3)
      case (Nil, Nil) => list3

  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def join(list1: List[A], list2: List[A], joined: List[A]): List[A] =
      (list1, list2) match
        case (h :: t, list2) => join(t, list2, h :: joined)
        case (Nil, h :: t) => join(Nil, t, h :: joined)
        case (Nil, Nil) => joined
    join(reverse(list2), reverse(list1), list3)

  def main(args: Array[String]) : Unit =
  {
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "'index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find(List("index11", "index0101", ""), "index2") == Nil)
    println(find(List("index1", "index2"), "") == Nil)
    println(find(Nil, "index012") == Nil)
    println(find(Nil, "") == Nil)

    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "'index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findTail(List("index11", "index0101", ""), "index2") == Nil)
    println(findTail(List("index1", "index2"), "") == Nil)
    println(findTail(Nil, "index012") == Nil)
    println(findTail(Nil, "") == Nil)

    println(findList(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "'index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findList(List("iindex1", "iindex2", "iindex222", "iindex73"), List("index1", "index2", "index3")) == List("iindex1", "iindex2", "iindex222"))
    println(findList(List("iindex12", "iindex2"), List("index1", "index12")) == List("iindex12"))
    println(findList(List("iindex1", "iindex2", "iindex222", "iindex23"), List("", "index3")) == Nil)
    println(findList(Nil, List("", "index1")) == Nil)
    println(findList(List("index1", "index2"), Nil) == Nil)
    println(findList(Nil, Nil) == Nil)

    println(findListTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "'index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findListTail(List("iindex1", "iindex2", "iindex222", "iindex73"), List("index1", "index2", "index3")) == List("iindex1", "iindex2", "iindex222"))
    println(findListTail(List("iindex12", "iindex2"), List("index1", "index12")) == List("iindex12"))
    println(findListTail(List("iindex1", "iindex2", "iindex222", "iindex23"), List("", "index3")) == Nil)
    println(findListTail(Nil, List("", "index1")) == Nil)
    println(findListTail(List("index1", "index2"), Nil) == Nil)
    println(findListTail(Nil, Nil) == Nil)

    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists(Nil, List('a'), List('b', 'c')) == List('a', 'b', 'c'))
    println(joinLists(List("a"), Nil, List("b", "c", "d", "e")) == List("a", "b", "c", "d", "e"))
    println(joinLists(List(1, 2), List(7), Nil) == List(1, 2, 7))
    println(joinLists(Nil, Nil, Nil) == Nil)

    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListsTail(Nil, List('a'), List('b', 'c')) == List('a', 'b', 'c'))
    println(joinListsTail(List("a"), Nil, List("b", "c", "d", "e")) == List("a", "b", "c", "d", "e"))
    println(joinListsTail(List(1, 2), List(7), Nil) == List(1, 2, 7))
    println(joinListsTail(Nil, Nil, Nil) == Nil)
  }
}
