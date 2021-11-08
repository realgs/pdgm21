import scala.annotation.tailrec

object l4 {
  //zl. obl. = l
  //zl. pam. = l
  def reverse[A](l: List[A], result: List[A]): List[A] =
    l match
      case Nil => result
      case (hd :: tl) => reverse(tl, hd :: result)

//  1
  def contains(elem: String, pattern: String): Boolean =
    if elem == "" then false else
      (elem, pattern) match
        case(_, "") => true
        case("", _) => false
        case _ => {
          def iter(elemPart: String, patternPart: String): Boolean =
            (elemPart, patternPart) match
              case (_, "") => true
              case ("", _) => false
              case _ => if elemPart.head == patternPart.head then iter(elemPart.tail, patternPart.tail) else false
          iter(elem, pattern) || contains(elem.tail, pattern)
        }

  def find(list: List[String], pattern: String): List[String] =
    list match
      case (h::t) => if contains(h, pattern) == true then h :: find(t, pattern) else find(t, pattern)
      case Nil => Nil

  def findTail(list: List[String], pattern: String): List[String] =
    @tailrec
    def findInner(list: List[String], result: List[String]): List[String] =
      list match
        case (h::t) => if contains(h, pattern) then findInner(t, h :: result) else findInner(t, result)
        case Nil => result
    findInner(list, Nil).reverse

  //dla n fraz
  //sprawdza, czy wyraz zawiera jakikolwiek wzor
  def containsPatterns(elem: String, patterns: List[String]): Boolean =
    patterns match
      case (h::t) => if contains(elem, h) then true else containsPatterns(elem, t)
      case Nil => false

  def findPatterns(list: List[String], patterns: List[String]): List[String] =
    if patterns == Nil then list
    else list match
      case (h::t) => if containsPatterns(h, patterns) then h :: findPatterns(t, patterns) else findPatterns(t, patterns)
      case Nil => Nil

  def findPatternsTail(list: List[String], patterns: List[String]): List[String] =
    if patterns == Nil then list else {
      @tailrec
      def findInner(list: List[String], result: List[String]): List[String] =
        list match
          case (h :: t) => if containsPatterns(h, patterns) then findInner(t, h :: result) else findInner(t, result)
          case Nil => result

      findInner(list, Nil).reverse
    }

  //2
  //zl. obl. = l1 + l2 + 1
  //zl. pam. = l3 + l3+1 + l3+2 + ... + l3+l2 + l3+l2+1 + ... + l3+l2+l1 =
  // = (l2 + l1 + 1)l3 + (l1 + 1)l2 + (l1 + 1)l1 / 2 + (l2 + 1)l2 / 2
  def addLists[A](l1: List[A], l2: List[A], l3: List[A]): List[A] =
    (l1, l2) match
      case (h::t, _) => h :: addLists(t, l2, l3)
      case (Nil, h::t) => h :: addLists(Nil, t, l3)
      case _ => l3

  //zl. obl. = 2x( l1 + l2 ) + 1 //bo reverse
  //zl. pam. = l3 + l2 + l1 //nie odkładane na stos
  def addListsTail[A](l1: List[A], l2: List[A], l3: List[A]): List[A] =
    @tailrec
    def add[A](l1: List[A], l2: List[A], result: List[A]): List[A] =
      (l1, l2) match
        case (h::t, _) => add(t, l2,  h :: result)
        case (Nil, h::t) => add(Nil, t, h :: result)
        case _ => result
    add(l2.reverse, l1.reverse, l3)

  def main(args: Array[String]): Unit = {
    // ------------------ 1 -------------------------------------------------------------
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find(List(), "abc") == List())
    println(find(List("a", "b", "c"), "") == List("a", "b", "c"))
    println(find(List("aleala", "aala", "alaa", "aallaa"), "ala") == List("aleala", "aala", "alaa"))

    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findTail(List(), "abc") == List())
    println(findTail(List("a", "b", "c"), "") == List("a", "b", "c"))
    println(findTail(List("aleala", "aala", "alaa"), "ala") == List("aleala", "aala", "alaa"))

    println(findPatterns(List("a", "b"), List())== List("a", "b") )
    println(findPatterns(List(), List("a", "b")) == List())
    println(findPatterns(List("aleala", "aala", "alaa", "aalla","alle", "eela", "elaala"), List("ala", "ale")) == List("aleala", "aala", "alaa", "elaala"))

    println(findPatternsTail(List("a", "b"), List())== List("a", "b") )
    println(findPatternsTail(List(), List("a", "b")) == List())
    println(findPatternsTail(List("aleala", "aala", "alaa", "eela", "elaala"), List("ala", "ale")) == List("aleala", "aala", "alaa", "elaala"))

    // ------------------ 2 -------------------------------------------------------------
    println(addLists(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    println(addLists(List(1,2,3), List(4,5,6), Nil) == List(1,2,3,4,5,6))
    println(addLists(List(1,2,3),Nil, List(4,5,6)) == List(1,2,3,4,5,6))
    println(addLists(Nil, Nil, Nil) == Nil)

    println(addListsTail(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    println(addListsTail(List(1,2,3), List(4,5,6), Nil) == List(1,2,3,4,5,6))
    println(addListsTail(Nil, Nil, Nil) == Nil)
  }
}
