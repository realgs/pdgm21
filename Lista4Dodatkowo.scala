import scala.annotation.tailrec

object Lista4Dodatkowo {

  def reverseList[A](list: List[A]) = {
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

  def removeExtraSpaces(text: String): String = {
    def helper(text: String, goodText: String): String = {
      (text, goodText) match
        case ("", g) => g
        case (t, "") => if t.head == ' ' then helper(t.tail, goodText) else helper(t.tail, t.head.toString)
        case (t, g) => if t.head == ' ' && (t.tail == "" || t.tail.head == ' ') then helper(t.tail, g) else helper(t.tail, g + t.head.toString)
    }
    helper(text, "")
  }
  /*
  n -> ilość liter "text" m -> ilość liter w Stringu wynikowym

  złożoność obliczeniowaw: n

  złożoność pamięciowa: m
  ponieważ stosujemy rekursje ogonową, będziemy potrzebować jedynie miejsca na wynik
  */

  def fixTexts(texts: List[String]): List[String] = {
    texts match
      case Nil => Nil
      case h::t => removeExtraSpaces(h)::fixTexts(t)
  }
  /*
  n -> ilość elementów "texts"

  złożoność obliczeniowa: n * złożoność "removeExtraSpaces" (zależna od długości poszczególnych napisów

  złożoność pamięciowa: n + (n^2 +n)/2 = 1/2 n^2 + 3/2 n
  ponieważ musimy zapamiętać "n" glów (n) oraz "n" ogonów ((n^2 + n)/2)
  */

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
      case h::t => if ifTextContainsPhrase(searchText, h) then true
                    else singleTextManyPhrases(searchText, t)
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
    val fixedPhrases = fixTexts(searchedPhrases)
    searchTexts match
      case Nil => Nil
      case h::t => if singleTextManyPhrases(removeExtraSpaces(h), fixedPhrases) then h::searchTextsLookingForPhrases(t, fixedPhrases)
                    else searchTextsLookingForPhrases(t, fixedPhrases)
  }
  /*
  n - ilość elementów "searchTexts", m - ilość elementów "searchedPhrases"

  złożoność obliczeniowa: n * złożoność "singleTextManyPhrases" = n * m * złożoność "ifTextContainsPhrase" (zależne od długości poszczególnych teksów i fraz)

  złożoność pamięciowa: n + (n^2 + n)/2 = 1/2 n^2 + 3/2 n
  ponieważ musimy zapamiętać "n" glów (n) oraz "n" ogonów ((n^2 + n)/2)
  */

  def searchTextsLookingForPhrasesTailrec(searchTexts: List[String], searchedPhrases: List[String]): List[String] = {
    val fixedPhrases = fixTexts(searchedPhrases)
    @tailrec
    def helper(texts: List[String], phrases: List[String], result: List[String]): List[String] = {
      texts match
        case Nil => result
        case h::t => if singleTextManyPhrases(removeExtraSpaces(h), fixedPhrases) then helper(t, fixedPhrases, h::result)
                      else helper(t, fixedPhrases, result)
    }
    reverseList(helper(searchTexts, fixedPhrases, Nil))
  }
  /*
  n - ilość elementów "searchTexts", m - ilość elementów "searchedPhrases", r - ilość elementów w liście wynikowej "result" 0 < r < n

  złożoność obliczeniowa: n * złożoność "singleTextManyPhrases" + złożoność "reverseList" =
                          = n * m * złożoność "ifTextContainsPhrase" (zależne od długości poszczególnych teksów i fraz) + r

  złożoność pamięciowa r + złożoność "reverseList" = 2 r
  ponieważ stosujemy rekursje ogonową, będziemy potrzebować jedynie miejsca na liste wynikową
  */

  def main(args: Array[String]): Unit = {
    println(searchTextsLookingForPhrases(List("Ala ma a", "    Ala ma b     " , "         ", "Ala   ma   c"), List("     Ala    ma          ")))
    println(searchTextsLookingForPhrases(List("ma ba", "dom", "lkdnfkj a bbb"), List("       ", "     a   b    ", "m          ")))
    println()

    println(searchTextsLookingForPhrasesTailrec(List("Ala ma a", "    Ala ma b     " , "         ", "Ala   ma   c"), List("     Ala    ma          ")))
    println(searchTextsLookingForPhrasesTailrec(List("ma ba", "dom", "lkdnfkj a bbb"), List("       ", "     a   b    ", "m          ")))
    println()
  }
}
