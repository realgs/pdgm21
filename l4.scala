import scala.annotation.tailrec

object l4 {

  def rev[A](listIn: List[A]):List[A]= {
    @tailrec
    def revTail[A](listIn: List[A], listOut: List[A]): (List[A]) = {
      if listIn == Nil then listOut
      else revTail(listIn.tail, listIn.head :: listOut)
    }
    revTail(listIn, Nil)
  }
  /*
    Złożoność obliczeniowa: O(n) n-listIn.length
    Złożoność pamięciowa: O(1)
  */

  //zadanie 1
  def containsPhrase(word: String, phrase: String): Boolean = {
    @tailrec
    def recContains (word:String, tempWord: String, phrase:String, tempPhrase:String):Boolean=
      (tempWord, tempPhrase) match
        case (_, "") => true
        case ("", _) => false
        case (_, _) =>
          if tempWord.head == tempPhrase.head then recContains(word, tempWord.tail, phrase, tempPhrase.tail)
          else recContains(word.tail, word.tail, phrase, phrase)
    recContains(word, word, phrase, phrase)
  }

  /*
   Złożoność obliczeniowa: Pesymistyczna: O(n!) , Średnia: O(n) - n długość słowa
   Złożoność pamięciowa: O(1)
 */


  def containsOneOfThePhrases(word: String, phraseList: List[String]): Boolean = {
    phraseList match
      case Nil => false
      case hphrase :: tphrase => if containsPhrase(word, hphrase) then true else containsOneOfThePhrases(word, tphrase)
  }
  /*
    Złożoność obliczeniowa: O(n) n-phraseList.length
    Złożoność pamięciowa: O(n)  n-phraseList.length
  */

  def find(listIn: List[String], phraseList: List[String]): List[String] = {
    (listIn, phraseList) match
      case (_, Nil) => listIn
      case (Nil, _) => Nil
      case (hlist :: tlist, _) =>
        if containsOneOfThePhrases(hlist, phraseList) then hlist :: find(tlist, phraseList)
        else find(tlist, phraseList)
  }

  /*
  n-długość listy wyrazów
  m-długość listy fraz
  t-długość wyrazów

  Złożoność obliczeniowa: średnia: O(n*m*t)~O(n^3), pesymistyczna: O(m*n*t!)~O(n^2*n!)
  Złożoność pamięciowa: O(n^2)~O((n+1)*n/2)
  */

  def findTail(listIn: List[String], phraseList: List[String]): List[String] = {
    @tailrec
    def recFind(listIn: List[String], phraseList: List[String], result: List[String]): List[String] = {
      (listIn, phraseList) match
        case (_, Nil) => rev(listIn)
        case (Nil, _) => result
        case (hlist :: tlist, _) =>
          if containsOneOfThePhrases(hlist, phraseList) then recFind(tlist, phraseList, hlist :: result)
          else recFind(tlist, phraseList, result)
    }
    rev(recFind(listIn, phraseList, Nil))
  }

  /*
  Złożoność obliczeniowa: średnia: O(n*m*t)~O(n^3), pesymistyczna: O(m*n*t!)~O(n^2*n!)
  Złożoność pamięciowa: O(1)
  */

  //zadanie 2
  def join3Lists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (Nil, Nil, l3) => l3
      case (Nil, hl2 :: tl2, _) => hl2 :: join3Lists(Nil, tl2, list3)
      case (hl1 :: tl1, _, _) => hl1 :: join3Lists(tl1, list2, list3)
  }

  /*
  Złożoność obliczeniowa: liniowa O(n), gdzie n=list1.length+list2.length
  Złożoność pamięciowa: kwadratowa O(n^2)
      1+2+...+n+m= (1+n)*n/2 + m gdzie n=list1.length+list2.length, m=list3.length
  */

  def join3ListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def recJoin[A](list1: List[A], list2: List[A], list3: List[A], concatList: List[A]): List[A] = {
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => concatList
        case (Nil, Nil, hl3 :: tl3) => recJoin(Nil, Nil, tl3, hl3 :: concatList)
        case (Nil, hl2 :: tl2, _) => recJoin(Nil, tl2, list3, hl2 :: concatList)
        case (hl1 :: tl1, _, _) => recJoin(tl1, list2, list3, hl1 :: concatList)
    }
    rev(recJoin(list1, list2, list3, List()))
  }

  /*
  Złożoność obliczeniowa: liniowa O(n)
      O(n+n)=O(2n), , gdzie n=list1.length+list2.length+list3.length
  Złożoność pamięciowa: O(n), gdzie n=list1.length+list2.length+list3.length
    Dzięki rekursji ogonowej nie stakujemy danych na stosie i jedyną zajętą pamięcią,
    jest ostatnia najdłuższa lista składająca się z wszystkich elementów (3 list)
  */

  def main(args: Array[String]): Unit = {
    println(join3Lists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(join3Lists(Nil, Nil, Nil) == Nil)
    println(join3Lists(List("A", "B"), Nil, List("C")) == List("A", "B", "C"))
    println(join3ListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(join3ListsTail(Nil, Nil, Nil) == Nil)
    println(join3ListsTail(List("A", "B"), Nil, List("C")) == List("A", "B", "C"))

    println(find(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
      "index0169224"), List("index0168")) == List("iindex016802", "iindex0168211", "iindex0168210"))
    println(find(List("aba", "bab", "cac"), List()) == List("aba", "bab", "cac"))
    println(find(List("abba", "abaa", "cac"), List("aba")))
    println(find(List("abaa", "baba", "cac"), List("aa", "ba")) == List("abaa", "baba"))
    println(findTail(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
      "index0169224"), List("index0168")) == List("iindex016802", "iindex0168211", "iindex0168210"))
    println(findTail(List("aba", "bab", "cac"), List()) == List("aba", "bab", "cac"))
    println(findTail(List("abaa", "baba", "cac"), List("aa", "ba")) == List("abaa", "baba"))
  }
}

