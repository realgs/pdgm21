import scala.annotation.tailrec

object l4 {
  def main(args: Array[String]): Unit = {}

  // 1
  def isSubstring(s1: String, s2: String): Boolean =
    (s1, s2) match
      case (_, "") => true
      case ("", _) => false
      case (s1, s2) => if s1.head == s2.head then (s1.tail, s2.tail) match {
                                                    case (_, "") => true
                                                    case ("", _) => false
                                                    case (t1, t2) => t1.head == t2.head && isSubstring(t1, t2) || t1.head == s1.head && isSubstring(s1.tail, s2)
                                                  }
                       else isSubstring(s1.tail, s2)

  def find(list: List[String], elems: List[String]): List[String] =
    def findHelper(list: List[String], elems: List[String], original: List[String]): List[String] =
      (list, elems) match
        case (h1 :: t1, h2 :: t2) if isSubstring(h1, h2) => h1 :: findHelper(t1, original, original)
        case (h1 :: t1, h2 :: t2) => if t2 == Nil then findHelper(t1, original, original) else findHelper(list, t2, original)
        case _ => Nil
    findHelper(list, elems, elems)

  def reverseList[A](list: List[A]): List[A] =
    def reverseListHelper[A](acc: List[A], l: List[A]): List[A] =
      if l==Nil then acc
      else reverseListHelper(l.head::acc, l.tail)
    reverseListHelper(Nil, list)

  def findT(list: List[String], elems: List[String]): List[String] =
    @tailrec
    def findTHelper(list: List[String], elems: List[String], original: List[String], result: List[String]): List[String] =
      (list, elems) match
        case (h1 :: t1, h2 :: t2) if isSubstring(h1, h2) => findTHelper(t1, original, original, h1 :: result)
        case (h1 :: t1, h2 :: t2) => if t2 == Nil then findTHelper(t1, original, original, result) else findTHelper(list, t2, original, result)
        case _ => reverseList(result)
    findTHelper(list, elems, elems, Nil)
}
