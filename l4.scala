import scala.annotation.tailrec
object l4 {
  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def iter(list: List[A], revlist: List[A]): List[A] =
      if list == Nil then revlist
      else iter(list.tail, list.head :: revlist)
    iter(list, Nil)


  def contain(string: String, pattern :String): Boolean =
    (string, pattern) match
      case ("", "") => true
      case (_, "") => true
      case ("", _) => false
      case (_, _) =>  if string.head == pattern.head then contain(string.tail, pattern.tail)
                      else contain(string.tail, pattern)


  def containAll(string: String, patterns :List[String]): Boolean =
    patterns match
      case Nil => false
      case head :: tail => if !contain(string, head) then containAll(string, tail) else true


  def findSingle(list: List[String], pattern: String): List[String] =
    if list == Nil then Nil
    else if pattern == "" then Nil
    else if contain(list.head, pattern) then list.head :: findSingle(list.tail, pattern)
    else findSingle(list.tail, pattern)

  def findSingleTail(list: List[String], pattern: String): List[String] =
    @tailrec
    def findSingleTailIter(list: List[String], pattern:String, accum: List[String]):List[String] =
      if list == Nil then reverse(accum)
      else if pattern == "" then Nil
      else if contain(list.head, pattern) then findSingleTailIter(list.tail, pattern, list.head :: accum)
      else findSingleTailIter(list.tail, pattern, accum)
    findSingleTailIter(list, pattern, Nil)

  def findAll(list: List[String], patterns: List[String]): List[String] =
    if list == Nil then Nil
    else if containAll(list.head, patterns) then list.head :: findAll(list.tail, patterns)
    else findAll(list.tail, patterns)

  def findAllTail(list: List[String], patterns: List[String]): List[String] =
    @tailrec
    def findAllTailIter(list: List[String], patterns: List[String], accum: List[String]): List[String] =
      if list == Nil then reverse(accum)
      else if patterns == Nil then Nil
      else if containAll(list.head, patterns) then findAllTailIter(list.tail, patterns, list.head :: accum)
      else findAllTailIter(list.tail, patterns, accum)
    findAllTailIter(list, patterns, Nil)

  //zadanie 2
  //obliczeniowa O(n) n-suma dlugosci list
  //pamieciowa O(n)
  def listsMerge[A] (list1:List[A], list2:List[A], list3:List[A]): List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (head::tail, _, _) => head::listsMerge(tail, list2, list3)
      case (Nil, head::tail, _) => head::listsMerge(Nil, tail, list3)
      case (Nil, Nil, head::tail) => list3

  //obliczeniowa O(n) n-suma dlugosci list
  //pamieciowa O(1)
  def listsMergeTail[A] (list1:List[A], list2:List[A], list3:List[A]): List[A] =
    @tailrec
    def listsMergeIter(list1:List[A], list2:List[A], list3:List[A], accum:List[A]): List[A] =
      (list1, list2, list3) match
        case(Nil, Nil, Nil) => reverse(accum)
        case(head::tail, _, _) => listsMergeIter(tail, list2, list3, head::accum)
        case(Nil, head::tail, _) => listsMergeIter(Nil, tail, list3, head::accum)
        case(Nil, Nil, head::tail) => listsMergeIter(Nil, Nil, tail, head::accum)
    listsMergeIter(list1, list2, list3, Nil)

  def main(aargs: Array[String]): Unit = {
    println(findSingle(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findSingleTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findAll(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168","index")) == List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"))
    println(findAllTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168","index")) == List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"))
    println(findSingle(List("123456789", "123456", "123", "1", "7654321", "13"), "123") == List("123456789", "123456", "123"))
    println(findSingleTail(List("123456789", "123456", "123", "1", "7654321", "13"), "123") == List("123456789", "123456", "123"))
    println(findAll(List("123456789", "123456", "123", "1", "7654321", "13"), List("123","12")) == List("123456789", "123456", "123"))
    println(findAllTail(List("123456789", "123456", "123", "1", "7654123", "13"), List("123","12")) == List("123456789", "123456", "123", "7654123"))

  }
}
