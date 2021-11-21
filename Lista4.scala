import scala.annotation.tailrec

object Lista4 {

  def reverseList [A](list: List[A]) = {
    @tailrec
    def tailrecHelper (list: List[A], result: List[A]): List[A] = {
      list match
        case Nil => result
        case h::t => tailrecHelper(t, h::result)
    }
    tailrecHelper(list, Nil)
  }
  /*
  n -> długość listy

  złożoność obliczeniowa: n

  złożoność pamięciowa: n
  ponieważ stosujemy rekursje ogonową, będziemy potrzebować jedynie miejsca na liste wynikową
  */

  //zadanie 1

  def ifTextContainsPhrase(searchText: String, searchedPhrase: String): Boolean = {
    def helper(text: String, phrase: String): Boolean = {
      (text, phrase) match
        case (_, "") => true
        case ("", _) => false
        case (t, p) => if t.head != p.head then false else helper(t.tail, p.tail)
    }
    searchText match
      case "" => false
      case text => if helper(searchText, searchedPhrase) then true
                    else ifTextContainsPhrase(text.tail, searchedPhrase)
  }
  /*
  n - ilość liter "searchText", m - ilość liter "searchedPhrase"

  złożoność obliczeniowa: zależna od tego czy tekst zawiera szukaną frazę i w którym miejscu
                          pesymistycznie - wyszukiwana fraza jest dłuższa od tekstu
                                          (n + 1) * n/2 = 1/2 n^2 + 1/2 n
  złożoność pamięciowa: 1
  ponieważ stosujemy rekursje ogonową, a wynikiem jest Boolean
  */

  def singleTextManyPhrases(searchText: String, searchedPhrases: List[String]): Boolean = {
    searchedPhrases match
      case Nil => false
      case ""::t => singleTextManyPhrases(searchText, t)
      case h::t => if ifTextContainsPhrase(searchText, h) then true else singleTextManyPhrases(searchText, t)
  }
  /*
  n - ilość elementów "searchedPhrase"

  złożoność obliczeniowa: zależna od tego czy tekst zawiera którąś z fraz oraz którą
                          pesymisztycznie - tekst nie zawiera żadnej frazy
                          n * złożoność "ifTextContainsPhrase" (zależne od długości teksu i poszczególnych fraz)

  złożoność pamięciowa: 1
  ponieważ stosujemy rekursje ogonową, a wynikiem jest Boolean
  */

  def searchTextsLookingForPhrases(searchTexts: List[String], searchedPhrases: List[String]): List[String] = {
    searchTexts match
      case Nil => Nil
      case h::t => if singleTextManyPhrases(h, searchedPhrases) then h::searchTextsLookingForPhrases(t, searchedPhrases)
                    else searchTextsLookingForPhrases(t, searchedPhrases)
  }
  /*
  n - ilość elementów "searchTexts", m - ilość elementów "searchedPhrases"

  złożoność obliczeniowa: n * złożoność "singleTextManyPhrases" = n * m * złożoność "ifTextContainsPhrase" (zależne od długości poszczególnych teksów i fraz)

  złożoność pamięciowa: n + (n^2 + n)/2 = 1/2 n^2 + 3/2 n
  ponieważ musimy zapamiętać "n" glów (n) oraz "n" ogonów ((n^2 + n)/2)
  */

  def searchTextsLookingForPhrasesTailrec(searchTexts: List[String], searchedPhrases: List[String]): List[String] = {
    @tailrec
    def helper(texts: List[String], phrases: List[String], result: List[String]): List[String] = {
      texts match
        case Nil => result
        case h::t => if singleTextManyPhrases(h, searchedPhrases) then helper(t, searchedPhrases, h::result)
                      else helper(t, searchedPhrases, result)
    }
    reverseList(helper(searchTexts, searchedPhrases, Nil))
  }
  /*
  n - ilość elementów "searchTexts", m - ilość elementów "searchedPhrases", r - ilość elementów w liście wynikowej "result" 0 < r < n

  złożoność obliczeniowa: n * złożoność "singleTextManyPhrases" + złożoność "reverseList" =
                          = n * m * złożoność "ifTextContainsPhrase" (zależne od długości poszczególnych teksów i fraz) + r

  złożoność pamięciowa r + złożoność "reverseList" = 2 r
  ponieważ stosujemy rekursje ogonową, będziemy potrzebować jedynie miejsca na liste wynikową
  */

  //zadanie 2

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2) match
      case (h::t, _) => h::joinLists(t, list2, list3)
      case (Nil, h::t) => h::joinLists(Nil, t, list3)
      case (Nil, Nil) => list3
  }
  /*
  n -> długość pierwszej listy, m -> długość drugiej listy

  złożoność obliczeniowa: n + m

  złożoność pamięciowa: n + (n^2 + n)/2 + m + (m^2 + m)/2 = 1/2 (n^2 + m^2) + 3/2 (n + m)
  ponieważ musimy zapamiętać "n" głów (n), "n" ogonów ((n^2 + n)/2), "m" głów (m) oraz "m" ogonów ((m^2 + m)/2)
  */

  def joinListsTailrec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def tailrecHelper(list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] = {
      (list1, list2, list3) match
        case (h::t, _, _) => tailrecHelper(t, list2, list3, h::result)
        case (Nil, h::t, _) => tailrecHelper(Nil, t, list3, h::result)
        case (Nil, Nil, h::t) => tailrecHelper(Nil, Nil, t, h::result)
        case (Nil, Nil, Nil) => result
    }
    reverseList(tailrecHelper(list1, list2, list3, Nil))
  }
  /*
  n -> długość pierwszej listy, m -> długość drugiej listy, o -> długość trzeciej listy

  złożoność obliczeniowa: n + m + o + złożoność reverseList = 2(n + m + o)

  złożoność pamięciowa: n + m + o + złożoność reverseList = 2(n + m + o)
  ponieważ stosujemy rekursje ogonową, będziemy potrzebować jedynie miejsca na liste wynikową
  */

  def main(args: Array[String]): Unit = {
    //zadanie 1

    println(searchTextsLookingForPhrases(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222","index0168"), List("index0168")))
    println(searchTextsLookingForPhrases(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index016820", "index016922")))
    println(searchTextsLookingForPhrases(List(""), List("index0168", "222")))
    println(searchTextsLookingForPhrases(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List()))
    println(searchTextsLookingForPhrases(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("")))
    println()

    println(searchTextsLookingForPhrasesTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222","index0168"), List("index0168")))
    println(searchTextsLookingForPhrasesTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index016820", "index016922")))
    println(searchTextsLookingForPhrasesTailrec(List(""), List("index0168", "222")))
    println(searchTextsLookingForPhrasesTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List()))
    println(searchTextsLookingForPhrasesTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("")))
    println()

    //zadanie 2
    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)))
    println(joinLists(List(), List(), List()))
    println(joinLists(List('m', 'i'), List('c', 'h', 'a'), List('ł')))
    println()

    println(joinListsTailrec(List(5, 4, 3, 2), List(1, 0), List(9)))
    println(joinListsTailrec(List(), List(), List()))
    println(joinListsTailrec(List('m', 'i'), List('c', 'h', 'a'), List('ł')))
    println()
  }
}
