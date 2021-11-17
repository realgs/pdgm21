def myReverse[A](list: List[A]): List[A] =
  def myReverseTail[A](list: List[A], finalList: List[A]) : List[A] =
    list match
      case Nil => finalList
      case h::t => myReverseTail(t, h::finalList)
  myReverseTail(list, Nil)

def myContains(text: String, pattern: String): Boolean = {
  def myContainsIteration(text: String, pattern: String): Boolean = {
    def myContainsMatch(text: String, pattern: String): Boolean = {
      (text, pattern) match
        case (_, "") => true
        case ("", _) => false
        case (_, _) => if text.head == pattern.head then myContainsMatch(text.tail, pattern.tail) else false
    }
    text match
      case "" => false
      case _ => if !myContainsMatch(text, pattern) then myContainsIteration(text.tail, pattern) else true
  }
  myContainsIteration(text, pattern)
}

def myContainsFromList(text: String, patterns: List[String]): Boolean =
  patterns match
    case Nil => false
    case h::t => if !myContains(text, h) then myContainsFromList(text, t) else true




//zadanie 1 - rekurencja nieogonowa (N fraz)
def findN(list: List[String], s: List[String]): List[String] =
  list match
    case Nil => Nil
    case h::t => if myContainsFromList(h, s) then h::findN(t,s) else findN(t,s)

findN(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168"))
findN(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168", "22"))
findN(List("alamakota", "olamakota", "alamapsa", "kasiamapsa", "kasiamachomika", "olamachomika"), List("asia", "ola", "kota"))


//zadanie 1 - rekurencja ogonowa (N fraz)
def findN2(list: List[String], s: List[String]): List[String] =
  def findN2Tail(list: List[String], s: List[String], finalList: List[String]): List[String] =
    list match
      case Nil => myReverse(finalList)
      case h::t => if myContainsFromList(h, s) then findN2Tail(t,s, h::finalList) else findN2Tail(t,s, finalList)
  findN2Tail(list, s, Nil)

findN2(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168"))
findN2(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168", "22"))
findN2(List("alamakota", "olamakota", "alamapsa", "kasiamapsa", "kasiamachomika", "olamachomika"), List("asia", "ola", "kota"))




//zadanie 2 - rekurencja nieogonowa
def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
  (list1, list2, list3) match
    case (Nil, Nil, Nil) => Nil
    case (Nil, Nil, h :: t) => h :: joinLists(Nil, Nil, t)
    case (Nil, h :: t, list3) => h :: joinLists(Nil, t, list3)
    case (h :: t, list2, list3) => h :: joinLists(t, list2, list3)


joinLists(List(5,4,3,2), List(1,0), List(9))


//zadanie 2 - rekurencja ogonowa
def joinLists2[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
  def joinListsTail(list1: List[A], list2: List[A], list3: List[A], finalList: List[A]): List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => finalList
      case (Nil, Nil, h::t) => joinListsTail(Nil, Nil, t, h::finalList)
      case (Nil, h::t, list3) => joinListsTail(Nil, t, list3, h::finalList)
      case (h::t, list2, list3) => joinListsTail(t, list2, list3, h::finalList)
  myReverse(joinListsTail(list1, list2, list3, Nil))


joinLists2(List(5,4,3,2), List(1,0), List(9))
