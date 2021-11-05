import scala.annotation.tailrec

object lab4_Scala extends App {
  //ZADANIE 1 - POJEDYNCZY STRING
  def find(list: List[String], keyword: String): List[String] =
    def findIter(list: List[String], keyword: String, baseElemIndex: Int, keywordIndex: Int, result: List[String]): List[String] =
      if keyword == "" then return result

      if list == Nil then return result
      else if keywordIndex == keyword.length then findIter(list.tail, keyword, 0, 0, list.head::result)
      else if list.head.length == baseElemIndex then findIter(list.tail, keyword, 0, 0, result)
      else if list.head.charAt(baseElemIndex) == keyword.charAt(keywordIndex) then findIter(list, keyword, baseElemIndex + 1, keywordIndex + 1, result)
      else findIter(list, keyword, baseElemIndex + 1, keywordIndex, result)
    findIter(list, keyword, 0, 0, List())

  println("******ZADANIE 1 - POJEDYNCZY STRING******")
  println(find(List("ramka","halo", "test", "testowy_ciag_znakow", "pozdrawiam", "paradygmaty"), "ra"))
  println(find(List("test", "testowy", "lubie_testy", "nie_lubie_testow", "polecam"), "test"))
  println(find(List(), "test"))

  //ZLOZONOSC OBLICZENIOWA: A - DLUGOSC LISTY BAZOWEJ, B - DLUGOSC ELEMENTU W LISCIE WEJSCIOWEJ, C - DLUGOSC SLOWA KLUCZOWEGO
  //GDY JEDEN ELEMENT W LISCIE BAZOWEJ - ZLOZONOSC B*C
  //GDY DODAMY WIECEJ ELEMENTOW W LISCIE BAZOWEJ - ZLOZONOSC A*B*C (POWTARZAMY A RAZY DODATKOWO)
  //OSTATECZNIE ZLOZONOSC OBLICZENIOWA: O(A*B*C)
  //ZLOZONOSC PAMIECIOWA: STALA, BO REKURENCJA OGONOWA

  //ZADANIE 1 - N FRAZ
  def findHelper(list: List[String], keyword: String, baseElemIndex: Int, keywordIndex: Int, result: List[String]): List[String] =
    if keyword == "" then return result

    if list == Nil then return result
    else if keywordIndex == keyword.length then findHelper(list.tail, keyword, 0, 0, list.head::result)
    else if list.head.length == baseElemIndex then findHelper(list.tail, keyword, 0, 0, result)
    else if list.head.charAt(baseElemIndex) == keyword.charAt(keywordIndex) then findHelper(list, keyword, baseElemIndex + 1, keywordIndex + 1, result)
    else findHelper(list, keyword, baseElemIndex + 1, keywordIndex, result)

  def findN(list: List[String], keywordList: List[String]): List[String] =
    def findNIter(list: List[String], keywordList: List[String], baseElemIndex: Int, keywordIndex: Int, result: List[String]): List[String] =
      if (keywordList != Nil) then findNIter(list, keywordList.tail, 0, 0, result:::findHelper(list, keywordList.head, 0, 0, List()))
      else return result
    findNIter(list, keywordList, 0, 0, List())

  println()
  println("******ZADANIE 1 - N FRAZ******")
  println(findN(List("ramka","halo", "test", "testowy_ciag_znakow", "pozdrawiam", "paradygmaty"), List("ra", "zna")))
  println(findN(List("test", "testowy", "lubie_testy", "nie_lubie_testow", "polecam"), List("lubie", "pol")))
  println(findN(List(), List("test", "pozdrowionka")))

  //DO PIERWSZEGO PRZYPADKU DOCHODZI DLUGOSC LISTY SLOW KLUCZOWYCH - D
  //OSTATECZNIE ZLOZONOSC OBLICZENIOWA WYNOSI O(A*B*C*D), BO POWTARZAMY CZYNNOSCI JESCZE DODATKOWE D RAZY
  //ZLOZONOSC PAMIECIOWA TO O(1), BO KORZYSTAMY Z REKURENCJI OGONOWEJ

  //ZADANIE 2 - REKURENCJA ZWYKLA
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (hx :: tx, _, _) => hx :: joinLists(tx, list2, list3)
      case (Nil, hy :: ty, _) => hy :: joinLists(Nil, ty, list3)
      case (Nil, Nil, hz :: tz) => hz :: joinLists(Nil, Nil, tz)
      case _ => Nil

  //ZLOZONOSC OBLICZENIOWA: O(N) - DOKŁADNIE N+1 (LICZBA WSZYSTKICH ELEMENTÓW LIST)
  //ZLOZONOSC PAMIECIOWA: O(N^2) - DOKŁADNIE (N^2+N)/2 + 3N (BO MUSI ZAPAMIĘTAĆ 3 GŁOWY I OGON N + (N-1) + ... + 1)

  println()
  println("******ZADANIE 2 - REKURENCJA ZWYKLA******")
  println(joinLists(List(1, 2, 3), List(7), List(12, 13)))
  println(joinLists(List("a", "b"), List("c", "d"), List("e")))
  println(joinLists(List(), List("x"), List()))

  //ZADANIE 2 - REKURENCJA OGONOWA
  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def reverseIter[A](list: List[A], solution: List[A]): List[A] =
      if list == Nil then solution
      else reverseIter(list.tail, list.head::solution)
    reverseIter(list, Nil)
    //ZŁOŻONOŚĆ OBLICZENIOWA: LINIOWA (N), GDZIE N - DŁUGOŚĆ LISTY
    //ZŁOŻONOŚĆ PAMIĘCIOWA: STAŁA O(1) - REKURENCJA OGONOWA

  def joinListsTailrec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def joinListsIter[A](list1: List[A], list2: List[A], list3: List[A], accum: List[A]): List[A] =
      if list1 != Nil then joinListsIter(list1.tail, list2, list3, list1.head::accum)
      else if list2 != Nil then joinListsIter(Nil, list2.tail, list3, list2.head::accum)
      else if list3 != Nil then joinListsIter(Nil, Nil, list3.tail, list3.head::accum)
      else reverse(accum)
    joinListsIter(list1, list2, list3, List())

  println()
  println("******ZADANIE 2 - REKURENCJA OGONOWA******")
  println(joinListsTailrec(List(1, 2, 3), List(7), List(12, 13)))
  println(joinListsTailrec(List("a", "b"), List("c", "d"), List("e")))
  println(joinListsTailrec(List(), List("x"), List()))

  //ZLOZONOSC OBLICZENIOWA: O(N) - WYKONA SIĘ DOKŁADNIE N RAZY I DODATKOWO UŻYWAMY METODY REVERSE,
  //KTÓRA WYWOŁANA ZOSTANIE RÓWNIEŻ N RAZY, WIĘC DOKŁADNIE ŁĄCZNIE 2N
  //ZLOZONOSC PAMIECIOWA: O(1) - DZIĘKI REKURENCJI OGONOWEJ STAŁA
}
