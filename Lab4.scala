package paradygmaty
import scala.annotation.tailrec

object Main {

  //Zadanie 1

  //rekurencja zwykła
  //Złożoność obliczeniowa O(n*(m+k)) n- długość listy, m - średnia długość słów w liście, k - długość stringa
  //Złożoność pamięciowa O(n) - zwykła rekurencja, wywoływanie find i czekanie, żeby dokleić
  def find(list: List[String], string: String): List[String] =
    if list == Nil then Nil
    else if stringContainsOther(list.head, string) then list.head :: find(list.tail, string)
    else find(list.tail, string)


  //rekurencja zykła - N fraz
  //Złożoność obliczeniowa O(n*j*(m+k)) n,j- długość list, m - średnia długość słów w liście, k - długość słów w 2 liście
  //Złożoność pamięciowa O(n) - zwykła rekurencja, wywoływanie find i czekanie, żeby dokleić
  def findList(list: List[String], otherList: List[String]): List[String] =
    if list == Nil then Nil
    else if listInString(list.head, otherList) then list.head :: findList(list.tail, otherList)
    else findList(list.tail, otherList)


  //rekurencja ogonowa
  //Złożoność obliczeniowa O(n*(m+k)) n- długość listy, m - średnia długość słów w liście, k - długość stringa
  //Złożoność pamięciowa O(1) - ogonowa
  def findStringTail(list: List[String], string: String): List[String] =
    @scala.annotation.tailrec
    def findStringTailHelper(result: List[String], strings: List[String], word: String): List[String] =
      if strings == Nil then result
      else if stringContainsOther(strings.head, word) then findStringTailHelper(strings.head :: result, strings.tail, word)
      else findStringTailHelper(result, strings.tail, word)
    findStringTailHelper(Nil, reverse(list), string)

  //rekurencja ogonowa- N fraz
  //Złożoność obliczeniowa O(n*j*(m+k)) n,j- długość list, m - średnia długość słów w liście, k - długość słów w 2 liście
  //Złożoność pamięciowa O(1) - ogonowa
  def findStringListTail(list: List[String], secondList: List[String]): List[String] =
    @scala.annotation.tailrec
    def findStringListTailHelper(result: List[String], strings: List[String], pattern: List[String]): List[String] =
      if strings == Nil then result
      else if listInString(strings.head, pattern) then findStringListTailHelper(strings.head :: result, strings.tail, pattern)
      else findStringListTailHelper(result, strings.tail, pattern)
    findStringListTailHelper(Nil, reverse(list), secondList)


  //Zadanie 2

  // Złożoność oblczeniowa O(n+m) - n,m - długości pierwszej i drugiej listy
  // Złożoność pamięciowa O(n+m)
  def joinLists[A] (firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
    (firstList, secondList, thirdList) match
      case (head::tail,_ ,_) => head :: joinLists(tail, secondList, thirdList)
      case (Nil, head::tail, _) => head :: joinLists(firstList, tail, thirdList)
      case (_, Nil, _) => thirdList

  //Złożoność obliczeniowa O(n+m+k) - n,m,k - długości list 1, 2 i 3
  //Złożoność pamięciowa O(1) - rekurencja ogonowa
  def joinListsWithTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
    @scala.annotation.tailrec
    def joinListsHelper[A] (result: List[A], firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
      (firstList, secondList, thirdList) match
        case (Nil, Nil, Nil) => reverse(result)
        case (head::tail, _, _) => joinListsHelper(head :: result, tail, secondList, thirdList)
        case (Nil, head::tail, _) => joinListsHelper(head :: result, firstList, tail, thirdList)
        case (Nil, Nil, head::tail) => joinListsHelper(head :: result, firstList, secondList, tail)
    joinListsHelper(Nil, firstList, secondList, thirdList)


  //Funkcje pomocnicze

  //Złożonośc obliczeniowa O(n), n - długość listy
  //Złożoność pamięciowa O(1) - rekurencja ogonowa
  def reverse[A](list: List[A]):List[A] =
    @scala.annotation.tailrec
    def reverseHelper[A] (listToReverse: List[A], result: List[A]): List[A] =
      listToReverse match
        case Nil => result
        case _ => reverseHelper(listToReverse.tail, listToReverse.head::result)
    reverseHelper(list, Nil)

  //Złożoność obliczeniowa O (n+m) - n - długość pierwszego, m - długość drugiego stringa
  //Złożoność pamięciowa O(1) - rekurencja ogonowa
  def stringContainsOther(firstString: String, otherString: String): Boolean =
    @scala.annotation.tailrec
    def stringContainsHelper(word: String, otherWord: String, text: String): Boolean=
      if otherWord.isEmpty then true
      else if word.isEmpty then false
      else if word.head == otherWord.head then
        stringContainsHelper(word.tail, otherWord.tail, text)
      else stringContainsHelper(text.tail, otherString, text.tail)
    stringContainsHelper(firstString, otherString, firstString)

  //Złożoność obliczeniowa, w najgorszym wypadku O(n* złożoność stringContainsOther)
  // = O(n*(n+m)) gdzie n długość stringa, m - max długość string z listy
  //Złożoność pamięciowa O(2) - jedno na listInString, drugie na stringContainsOther
  @scala.annotation.tailrec
  def listInString(string: String, list: List[String]): Boolean =
    if list == Nil then false
    else if stringContainsOther(string, list.head) then true
    else listInString(string, list.tail)




  def main(args: Array[String]): Unit = {

    val listForTests = List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224")
    val resultList = List("iindex0168202", "iindex0168211", "iindex0168210")
    val indexToFind = "index0168"


    println()
    println("testy zad 1:")
    println("testy zwykłej rekurencji jedna fraza: ")
    println(find(listForTests, indexToFind) == resultList)
    println(find(listForTests, "") == listForTests)
    println(find(List(), indexToFind) == List())

    println("testy zwykłej rekurencji N fraz:")


    println()
    println("testy zad 2:")
    println("rekurencja zwykła:")
    println( joinLists(List(5,4,3,2), List(1,0), List(9)) == List(5,4,3,2,1,0,9))
    println( joinLists( List(), List(1,2), List(3,4)) == List(1,2,3,4))
    println( joinLists( List(1,2), List(3,4), List()) == List(1,2,3,4))
    println( joinLists( List(1,2), List(), List(3,4)) == List(1,2,3,4))
    println( joinLists( List(), List(), List()) == List())

    println()
    println("rekurencja ogonowa:")
    println( joinListsWithTail(List(5,4,3,2), List(1,0), List(9)) == List(5,4,3,2,1,0,9))
    println( joinListsWithTail( List(), List(1,2), List(3,4)) == List(1,2,3,4))
    println( joinListsWithTail( List(1,2), List(3,4), List()) == List(1,2,3,4))
    println( joinListsWithTail( List(1,2), List(), List(3,4)) == List(1,2,3,4))
    println( joinListsWithTail( List(), List(), List()) == List())

    println()
    println("testy funkcji pomocnicznych:")
    println()

    println("test reverse:")
    println( reverse(List(1,2,3)) == List(3, 2, 1) )
    println( reverse(List()) == List() )

    println()
    println("test stringContainsOther:")
    println(stringContainsOther("testdzialania", "dzia") == true)
    println(stringContainsOther("testdzialania", "dzien") == false)
    println(stringContainsOther("testSecondStringEmpty", "") == true)
    println(stringContainsOther("", "testFirstStringEmpty") == false)

    println()
    println("test listInString")
    println( listInString("EmptyListTest", List()) == false )
    println( listInString("testss", List("yes", "this", "test", "should", "work")) == true)
    println( listInString("test", List("no", "this", "should", "not", "work")) == false)


  }

}

