//Piotr Zięba
object Main {
  def reverseTailRec[A] (list: List[A]) =
    def reverseTailRecI[A](list: List[A], result: List[A]): List[A] =
      list match
        case Nil => result
        case h::t => reverseTailRecI(t, h::result)
    reverseTailRecI(list, Nil)

  //Zadanie 1
  def contains(searched: String, toBeFound: String) =
    def containsI(searchedPart: String, toBeFoundPart: String): Boolean =
      (searchedPart, toBeFoundPart) match
        case ("", "") => true
        case ("", _) => false
        case (_, "") => true
        case _ =>
          if searchedPart.head == toBeFoundPart.head then containsI(searchedPart.tail, toBeFoundPart.tail)
          else containsI(searchedPart.tail, toBeFound)
    containsI(searched, toBeFound)

  def containsAnyOfList(searched: String, toBeFoundList: List[String]): Boolean =
    toBeFoundList match
      case Nil => false
      case h::t =>
        if contains(searched, h) then true
        else containsAnyOfList(searched, t)

  def foundInAnyTailRec(searchedList: List[String], toBeFoundList: List[String]) =
    def foundInAnyTailRecI(searchedList: List[String], foundIn: List[String]): List[String] =
      searchedList match
        case Nil => foundIn
        case h::t =>
          if containsAnyOfList(h, toBeFoundList) then foundInAnyTailRecI(t, h::foundIn)
          else foundInAnyTailRecI(t, foundIn)
    reverseTailRec(foundInAnyTailRecI(searchedList, Nil))

  def foundInAnyRec(searchedList: List[String], toBeFoundList: List[String]): List[String] =
    searchedList match
      case Nil => Nil
      case h::t =>
        if containsAnyOfList(h, toBeFoundList) then h::foundInAnyRec(t, toBeFoundList)
        else foundInAnyRec(t, toBeFoundList)

  //Zadanie 2
  def join3ListsRec[A] (list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (h1::t1, _, _) => h1::join3ListsRec(t1, list2, list3)
      case (_, h2::t2, _) => h2::join3ListsRec(Nil, t2, list3)
      case (_, _, h3::t3) => h3::join3ListsRec(Nil, Nil, t3)

  def join3ListsTailRec[A] (list1: List[A], list2: List[A], list3: List[A]) =
    def join3ListsTailRecI[A] (list1: List[A], list2: List[A], list3: List[A], joined: List[A]): List[A] =
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => reverseTailRec(joined)
        case (h1::t1, _, _) => join3ListsTailRecI(t1, list2, list3, h1::joined)
        case (_, h2::t2, _) => join3ListsTailRecI(Nil, t2, list3, h2::joined)
        case (_, _, h3::t3) => join3ListsTailRecI(Nil, Nil, t3, h3::joined)
    join3ListsTailRecI(list1, list2, list3, Nil)

  def main(args: Array[String]): Unit = {
    //Zadanie 1 TailRec
    println(foundInAnyTailRec(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210"))
    println(foundInAnyTailRec(List("janek", "anna", "olgierd", "igi"), List("an", "gier")) == List("janek", "anna", "olgierd"))
    println(foundInAnyTailRec(List(), List()) == List())

    //Zadanie 1 Rec
    println(foundInAnyRec(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168")) == List("index0168202", "index0168211", "index0168210"))
    println(foundInAnyRec(List("janek", "anna", "olgierd", "igi"), List("an", "gier")) == List("janek", "anna", "olgierd"))
    println(foundInAnyRec(List(), List()) == List())

    //Zadanie 2 TailRec
    println(join3ListsTailRec(List(), List(), List()) == List())
    println(join3ListsTailRec(List(5, 4, 3, 2), List(), List(9)) == List(5, 4, 3, 2, 9))
    println(join3ListsTailRec(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))

    //Zadanie 2 Rec
    println(join3ListsRec(List(), List(), List()) == List())
    println(join3ListsRec(List(5, 4, 3, 2), List(), List(9)) == List(5, 4, 3, 2, 9))
    println(join3ListsRec(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
  }
}

