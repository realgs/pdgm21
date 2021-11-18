import scala.annotation.tailrec

object l4 {

  //custom reverse function
  def reverse[A](list: List[A]): List[A] =
    def iter[A](list: List[A], acc: List[A]): List[A] =
      list match
        case(Nil)    => acc
        case(h :: t) => iter(t, h :: acc)
    iter(list, Nil)

  //zadanie 1

  def compareWithPattern[A](word: String, pattern: String): Boolean =
    (word, pattern) match
      case(_, "") => true
      case("", _) => false
      case(_, _)  => if word.head == pattern.head then compareWithPattern(word.tail, pattern.tail) else false

  def checkOneWordWithOnePattern[A](word: String, pattern: String): Boolean =
    (word, pattern) match
      case(_, "") => true
      case("", _) => false
      case(_, _)  => if compareWithPattern(word, pattern) == true then true else checkOneWordWithOnePattern(word.tail, pattern)

  def checkOneWordWithPatternsList[A](word: String, patternsList: List[String]): Boolean =
    if patternsList == Nil then false
    else if checkOneWordWithOnePattern(word, patternsList.head) == true then true
    else checkOneWordWithPatternsList(word, patternsList.tail)

  def find[A](wordsList: List[String], patternsList: List[String]): List[String] =
    (wordsList, patternsList) match
      case(Nil, _)    => Nil
      case(_, Nil)    => wordsList
      case(h :: t, p) => if checkOneWordWithPatternsList(h, p) then h :: find(t, p) else find(t, p)


  def find_T[A](wordsList: List[String], patternsList: List[String]): List[String] =
    @tailrec
    def iter[A](wordsList: List[String], patternsList: List[String], acc: List[String]): List[String] =
      (wordsList, patternsList) match
        case(Nil, _)    => reverse(acc)
        case(_, Nil)    => wordsList
        case(h :: t, p) => if checkOneWordWithPatternsList(h, p) then iter(t, p, h :: acc) else iter(t, p, acc)
    iter(wordsList, patternsList, Nil)

  //zadanie 2

  def joinLists[A](l1: List[A], l2: List[A], l3: List[A]): List[A] =
    (l1, l2, l3) match
      case(Nil, Nil, Nil)    => Nil
      case(Nil, Nil, h :: t) => h :: joinLists(Nil, Nil, t)
      case(Nil, h :: t, _)   => h :: joinLists(Nil, t, l3)
      case(h :: t, _, _)     => h :: joinLists(t, l2, l3)

  def joinLists_T[A](l1: List[A], l2: List[A], l3: List[A]): List[A] =
    @tailrec
    def iter[A](l1: List[A], l2: List[A], l3: List[A], acc: List[A]): List[A] =
      (l1, l2, l3) match
        case(Nil, Nil, Nil)    => reverse(acc)
        case(Nil, Nil, h :: t) => iter(Nil, Nil, t, h :: acc)
        case(Nil, h :: t, _)   => iter(Nil, t, l3, h :: acc)
        case(h :: t, _, _)     => iter(t, l2, l3, h :: acc)
    iter(l1, l2, l3, Nil)

  def main(args: Array[String]): Unit = {
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find(List(), List("2137", "7312")) == List())
    println(find(List("123", "456", "789"), List()) == List("123", "456", "789"))
    println(find(List("1211134", "456789", "12321", "7622245", "11123"), List("111", "222")) == List("1211134", "7622245", "11123"))

    println(find_T(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find_T(List(), List("2137", "7312")) == List())
    println(find_T(List("123", "456", "789"), List()) == List("123", "456", "789"))
    println(find_T(List("1211134", "456789", "12321", "7622245", "11123"), List("111", "222")) == List("1211134", "7622245", "11123"))

    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists(List(), List(), List()) == List())
    println(joinLists(List(1, 2, 3), List(), List(4, 5, 6, 7)) == List(1, 2, 3, 4, 5, 6, 7))

    println(joinLists_T(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists_T(List(), List(), List()) == List())
    println(joinLists_T(List(1, 2, 3), List(), List(4, 5, 6, 7)) == List(1, 2, 3, 4, 5, 6, 7))
  }
}
