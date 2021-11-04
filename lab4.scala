import scala.annotation.tailrec

object lab4 {

  //Zadanie 1

  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def go(list: List[A], output: List[A]): List[A] =
      if list == Nil then output
      else go(list.tail, list.head::output)
    go(list, Nil)

  @tailrec
  def contains(listElement: String, phrase: String): Boolean =
    (listElement, phrase) match
      case ("", "") => true
      case ("", _) => false
      case (_, "") => true
      case (_, _) =>  if (listElement.head == phrase.head) then contains(listElement.tail, phrase.tail)
                      else contains(listElement.tail, phrase)

  def containsSinglePhrase(list: List[String], phrase: String): List[String] =
    if list == Nil then Nil
    else if (phrase != "" && contains(list.head, phrase)) then list.head :: containsSinglePhrase(list.tail, phrase)
    else containsSinglePhrase(list.tail, phrase)

  def containsSinglePhraseTailRec(list: List[String], phrase: String): List[String] =
    @tailrec
    def go(list: List[String], phrase: String, acc: List[String]): List[String] =
      if list == Nil then acc.reverse
      else if (phrase != "" && contains(list.head, phrase)) then go(list.tail, phrase, list.head::acc)
      else go(list.tail, phrase, acc)
    go(list, phrase, Nil)

  def containsMultiplePhrase(list: List[String], phraseList: List[String]): List[String] =
    @tailrec
    def go(listElement: String, phraseList: List[String]): Boolean =
      if phraseList == Nil then false
      else if (phraseList.head != "" && contains(listElement, phraseList.head)) then true else go(listElement, phraseList.tail)
    if list == Nil then Nil
    else if go(list.head, phraseList) then list.head::containsMultiplePhrase(list.tail, phraseList)
    else containsMultiplePhrase(list.tail, phraseList)

  //Zadanie 2

  def joinLists[A](listA: List[A], listB: List[A], listC: List[A]): List[A] =
    (listA, listB, listC) match
      case (h::t, _, _) => h::joinLists(t, listB, listC)
      case (Nil, h::t, _) => h::joinLists(Nil, t, listC)
      case (Nil, Nil, _) => listC

  def joinListsTailRec[A](listA: List[A], listB: List[A], listC: List[A]): List[A] =
    @tailrec
    def go[A](listA: List[A], listB: List[A], listC: List[A], acc: List[A]): List[A] =
      (listA, listB, listC) match
        case (h::t, _, _) => go(t, listB, listC, h::acc)
        case (Nil, h::t, _) => go(Nil, t, listC, h::acc)
        case (Nil, Nil, h::t) => go(Nil, Nil, t, h::acc)
        case (Nil, Nil, Nil) => acc.reverse
    go(listA, listB, listC, Nil)


  def main(args: Array[String]): Unit = {

    println(joinLists(Nil, Nil, Nil) == Nil)
    println(joinListsTailRec(Nil, Nil, Nil) == Nil)
    println(joinLists(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    println(joinListsTailRec(List(1,2,3), List(4,5,6), List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    println(contains("Ala ma kota", "ma") == true)
    println(contains("ma", "ma"))
    println(contains("karma", "ma"))
    //- - - - - - - - - - - - - - - - - - - -
    println(containsSinglePhrase(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168")
                              == List("iindex0168202","iindex0168211","iindex0168210"))
    println(containsSinglePhraseTailRec(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168")
      == List("iindex0168202","iindex0168211","iindex0168210"))
    //- - - - - - - - - - - - - - - - - - - -
    println(containsSinglePhrase(List("Ala", "ma", "kota", "ma", "psa", "mama", "karma", "matma", "magma", "dom", "piwo"), "ma")
      == List("ma", "ma", "mama", "karma", "matma", "magma"))
    println(containsSinglePhrase(List("Ala", "ma", "kota"), "") == Nil)
    println(containsSinglePhraseTailRec(List("Ala", "ma", "kota"), "") == Nil)
    println(containsMultiplePhrase(List("Ala", "ma", "kota"), List("sds", "dfd", "")) == Nil)
    println(containsSinglePhraseTailRec(List("Ala", "ma", "kota", "ma", "psa", "mama", "karma", "matma", "magma", "dom", "piwo"), "ma")
    == List("ma", "ma", "mama", "karma", "matma", "magma"))
    println(containsMultiplePhrase(List("Ala", "ma", "kota", "ma", "psa", "mama", "karma", "matma", "magma", "dom", "piwo"),
      List("Ala", "ma", "kota"))
      == List("Ala", "ma", "kota", "ma", "mama", "karma", "matma", "magma"))

  }


}
