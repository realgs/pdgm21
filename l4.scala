object l4 {

def find(list: List[String], element: String): List[String] = {
  list match
    case Nil => Nil
    case h :: t => if pattern(h, element) then h :: find(t, element) else find(t, element)
}

def findTail(list: List[String], pattern: String): List[String] = {
  def findTailIter(listOfElem: List[String], matchList: List[String]): List[String] =
    listOfElem match
      case Nil => matchList
      case h :: t => if patternTail(h, pattern) then findTailIter(listOfElem.tail, h :: matchList)
        else findTailIter(listOfElem.tail, matchList)
  findTailIter(list, Nil)
}

def pattern(elem: String, patt: String): Boolean = {
  def patternIter(text: String, pattText: String): Boolean =
    (text, pattText) match
      case (_, "") => true
      case ("", _) => false
      case _ => if text.head == pattText.head then patternIter(text.tail, pattText.tail)
        else if text.head == patt.head then patternIter(text, patt)
        else patternIter(text.tail, patt)
  patternIter(elem, patt)
}

def patternTail(elem: String, patt: String): Boolean = {
  def patternTailIter(text: String, pattText: String, textT: String): Boolean =
    (text, pattText) match
      case (_, "") => true
      case ("", _) => false
      case _ => if text.head == pattText.head then patternTailIter(text.tail, pattText.tail, textT)
        else patternTailIter(textT.tail, patt, textT.tail)
  patternTailIter(elem, patt, elem)
}

//task 2
def joinLists(list1: List[Int], list2: List[Int], list3: List[Int]): List[Int] = {
  (list1, list2, list3) match
    case (Nil, Nil, Nil) => Nil
    case (Nil, _, _) => joinLists(list2, list3, Nil)
    case (_, Nil, Nil) => list1
    case (h :: t, _, _) => h :: joinLists(t, list2, list3)
}


def joinListsTail(list1: List[Int], list2: List[Int], list3: List[Int]): List[Int] = {
  def joinListsTailIter(l1: List[Int], l2: List[Int], l3: List[Int], united: List[Int]): List[Int] = {
    (l1, l2, l3) match
      case (Nil, Nil, Nil) => united
      case (h :: t, _ , _) => joinListsTailIter(t, l2, l3, h :: united)
      case (Nil, _, _) => joinListsTailIter(l2, l3, Nil, united)
  }
  reverse(joinListsTailIter(list1, list2, list3, Nil))
}

def reverse(list: List[Int]): List[Int] = {
  def reverseIter(toReverse: List[Int], result: List[Int]): List[Int] =
    toReverse match
      case Nil => result
      case h :: t => reverseIter(t, h :: result)
  reverseIter(list, Nil)
}


  def main(args: Array[String]): Unit = {
    println("find")
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "sss"))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index"))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "222"))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), ""))
    println(find(List(), "x"))

    println("\nfindTail")
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "sss"))
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index"))
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "222"))
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), ""))
    println(findTail(List(), "x"))

    println("\njoinLists")
    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)))
    println(joinLists(List(5, 4, 3, 2), List(1, 0), Nil))
    println(joinLists(Nil, List(1, 0), List(9)))
    println(joinLists(Nil, List(1, 0), Nil))
    println(joinLists(List(5, 4, 3, 2), Nil, List(9)))
    println(joinLists(List(5, 4, 3, 2), Nil, Nil))
    println(joinLists(Nil, Nil, Nil))

    println("\njoinListsTail")
    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)))
    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), Nil))
    println(joinListsTail(Nil, List(1, 0), List(9)))
    println(joinListsTail(Nil, List(1, 0), Nil))
    println(joinListsTail(List(5, 4, 3, 2), Nil, List(9)))
    println(joinListsTail(List(5, 4, 3, 2), Nil, Nil))
    println(joinListsTail(Nil, Nil, Nil))


  }
}
