package Lab4
import scala.annotation.tailrec

object lab4 {

  //Zadanie 1

  def find(list : List[String], elements : List[String]) : List[String] =
    if list == Nil then Nil
    else if elements == Nil then list
    else if containsList(list.head, elements) then list.head :: find(list.tail, elements)
    else find(list.tail, elements)

  def contains(string : String, element : String) : Boolean =
    (element, string) match
      case ("", _) => true
      case (_, "") => false
      case (_, _) => if  string.head == element.head then contains(string.tail, element.tail) else contains(string.tail, element)

  def containsList(string: String, elements : List[String]) : Boolean =
    elements match
      case Nil => false
      case h :: t => if contains(string, h) then true else containsList(string, t)

  def reverse[A](list : List[A]) : List[A] =
    @tailrec
    def helper(list : List[A], acc : List[A]) : List[A] =
      list match
        case Nil => acc
        case h :: t => helper(t, h :: acc)
    helper(list, Nil)

  def findTail(list : List[String], elements : List[String]) : List[String] =
    @tailrec
    def findHelper(list : List[String], elements : List[String], acc : List[String]) : List[String] =
      if list == Nil then acc.reverse
      else if elements == Nil then list
      else if containsListTail(list.head, elements) then findHelper(list.tail, elements, list.head :: acc)
      else findHelper(list.tail, elements, acc)
    findHelper(list, elements, Nil)

  @tailrec
  def containsTail(string : String, element : String) : Boolean =
    (element, string) match
      case ("", _) => true
      case (_, "") => false
      case (_, _) => if  string.head == element.head then containsTail(string.tail, element.tail) else containsTail(string.tail, element)

  @tailrec
  def containsListTail(string: String, elements : List[String]) : Boolean =
    elements match
      case Nil => false
      case h :: t => if containsTail(string, h) then true else containsListTail(string, t)

  //Zadanie 2

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match {
      case (Nil, Nil, Nil) => Nil
      case (Nil, Nil, _) => list3
      case (Nil, h :: t, _) => h :: joinLists(list1, t, list3)
      case (h :: t, _, _) => h :: joinLists(t, list2, list3)
    }

  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def join[A](list1: List[A], list2: List[A], list3: List[A], acc: List[A]) : List[A] =
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => acc.reverse
        case (Nil, Nil, h :: t) => join(Nil, Nil, t, h :: acc)
        case (Nil, h :: t, _) => join(Nil, t, list3, h :: acc)
        case (h :: t, _, _) => join(t, list2, list3, h :: acc)
      }
    join(list1, list2, list3, Nil)

  def main(args : Array[String]) : Unit = {
    println("\njoinLists")
    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists(List(), List(), List()) == List())
    println(joinLists(List(), List(1, 2), List(4)) == List(1, 2, 4))

    println("\njoinListsTail")
    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListsTail(List(), List(), List()) == List())
    println(joinListsTail(List(), List(1, 2), List(4)) == List(1, 2, 4))

    println("\nfind")
    println(find(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("211", "69")) == List("index0169", "iindex0168211", "iindex0169222"))
    println(find(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), Nil) == List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"))

    println("\nfindTail")
    println(findTail(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findTail(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("211", "69")) == List("index0169", "iindex0168211", "iindex0169222"))
    println(findTail(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), Nil) == List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"))
  }
}

