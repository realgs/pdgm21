import scala.annotation.tailrec

object Lista4 {

  def reverseList [A](list: List[A]) = {
    @tailrec
    def tailrecHelper (list: List[A], result: List[A]): List[A] = {
      list match
        case h::t => tailrecHelper(t, h::result)
        case Nil => result
    }
    tailrecHelper(list, Nil)
  }

  //zadanie 1

  def containsPhrase (word :String, phrase: String): Boolean = {
    (word, phrase) match
      case (_, "") => true
      case ("", _) => false
      case (w, p) => if w.head == p.head then containsPhrase(w.tail, p.tail) else containsPhrase(w.tail, phrase)
  }

  def find (words: List[String], phrase: String): List[String] = {
    words match
      case Nil => Nil
      case h::t => if containsPhrase(h, phrase) then h::find(t, phrase) else find(t, phrase)
  }

  def findTailrec (words: List[String], phrase: String) = {
    @tailrec
    def tailrecHelper (words: List[String], result: List[String]): List[String] = {
      words match
        case Nil => result
        case h::t => if containsPhrase(h, phrase) then tailrecHelper(t, h::result) else tailrecHelper(t, result)
    }
    reverseList(tailrecHelper(words, Nil))
  }

  def findN (words: List[String], phrases: List[String]) = {
    def recHelper (words:List[String], phrasesLeft: List[String]): List[String] = {
      (words, phrasesLeft) match
        case (Nil, _) => Nil
        case (h::t, Nil) => recHelper(t, phrases)
        case (h1::t1, h2::t2) => if containsPhrase(h1, h2) then h1::recHelper(t1, phrases) else recHelper(words, t2)
    }
    recHelper(words, phrases)
  }

  def findNTailrec (words: List[String], phrases: List[String]) = {
    @tailrec
    def tailrecHelper (words:List[String], phrasesLeft: List[String], result: List[String]): List[String] = {
      (words, phrasesLeft) match
        case (Nil, _) => result
        case (h::t, Nil) => tailrecHelper(t, phrases, result)
        case (h1::t1, h2::t2) => if containsPhrase(h1, h2) then tailrecHelper(t1, phrases, h1::result) else tailrecHelper(words, t2, result)
    }
    reverseList(tailrecHelper(words, phrases, Nil))
  }

  //zadanie 2

  def joinLists [A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2) match
      case (h::t, _) => h::joinLists(t, list2, list3)
      case (Nil, h::t) => h::joinLists(Nil, t, list3)
      case (Nil, Nil) => list3
  }

  def joinListsTailrec [A](list1: List[A], list2: List[A], list3: List[A]) = {
    @tailrec
    def tailrecHelper (list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] = {
      (list1, list2, list3) match
        case (h::t, _, _) => tailrecHelper(t, list2, list3, h::result)
        case (Nil, h::t, _) => tailrecHelper(Nil, t, list3, h::result)
        case (Nil, Nil, h::t) => tailrecHelper(Nil, Nil, t, h::result)
        case (Nil, Nil, Nil) => result
    }
    reverseList(tailrecHelper(list1, list2, list3, Nil))
  }

  def main(args: Array[String]): Unit = {
    //zadanie 1 testy

    println(find(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), "index0168"))
    println(find(List(), "a"))
    println(find(List("zadanie", "drugie"), ""))
    println()

    println(findTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), "index0168"))
    println(findTailrec(List(), "a"))
    println(findTailrec(List("zadanie", "drugie"), ""))
    println()

    println(findN(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168")))
    println(findN(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index016820", "index016922")))
    println(findN(List(), List("index0168", "222")))
    println(findN(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List()))
    println(findN(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("")))
    println()

    println(findNTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168")))
    println(findNTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index016820", "index016922")))
    println(findNTailrec(List(), List("index0168", "222")))
    println(findNTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List()))
    println(findNTailrec(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("")))
    println()

    //zadanie 2 testy

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
