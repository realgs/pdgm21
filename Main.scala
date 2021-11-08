import scala.annotation.tailrec

object Main {

  def reverseList [A](list: List[A]) = {
    @tailrec
    def reverseHelper (list: List[A], result: List[A]): List[A] = {
      list match
        case h::t => reverseHelper(t, h::result)
        case Nil => result
    }
    reverseHelper(list, Nil)
  }

//             12345432  1234328695
  def contains(word: String, sentence: String): Boolean = {
    (word, sentence) match
      case ("", _) => true
      case (_, "") => false
      case (word, sentence) => if word.head == sentence.head then contains(word.tail, sentence.tail) else contains(word, sentence.tail)
  }

  def find(strings: List[String], word: String): List[String] = {
    strings match
      case Nil => Nil
      case h::t => if contains(word, h) then h :: find(t, word) else find(t, word)
  }

  def findTail(strings: List[String], word: String) = {
    @tailrec
    def findHelper(strings: List[String], word: String, result: List[String]): List[String] = {
      strings match
        case Nil => result
        case h::t => if contains(word, h) then findHelper(t, word, h :: result) else findHelper(t, word, result)
    }
    reverseList(findHelper(strings, word, Nil))
  }

  def findList(strings: List[String], words: List[String]): List[String] = {
    (strings, words) match {
      case (Nil, _) => Nil
      case (_, Nil) => strings
      case (h::t, h2::t2) => if containsList(words, h) then h :: findList(t, words) else findList(t, words)
    }
  }

  def findListTail(strings: List[String], words: List[String]) = {
    @tailrec
    def findListTailHelper(strings: List[String], words: List[String], result: List[String]): List[String] = {
      (strings, words) match
        case (Nil, _) => result
        case (_, Nil) => strings
        case (h::t, h2::t2) => if containsList(words, h) then findListTailHelper(t, words, h :: result) else findListTailHelper(t, words, result)

    }
    reverseList(findListTailHelper(strings, words, Nil))
  }

  def containsList(words: List[String], sentence: String): Boolean = {
    words match
      case Nil => false
      case h::t => if contains(h, sentence) then true else containsList(t, sentence)
  }



  def joinLists[A](l1: List[A], l2: List[A], l3: List[A]): List[A] = {
    (l1, l2, l3) match
      case (Nil, Nil, Nil) => Nil
      case (h::t, _, _) => h :: joinLists(t, l2, l3)
      case (Nil, h2::t2, _) => h2 :: joinLists(Nil, t2, l3)
      case (Nil, Nil, h3::t3) => h3 :: joinLists(Nil, Nil, t3)

  }

  def joinListsTail[A](l1: List[A], l2: List[A], l3: List[A]) = {
    @tailrec
    def joinHelper[A](l1: List[A], l2: List[A], l3: List[A], result: List[A]): List[A] = {
      (l1, l2, l3) match {
        case (Nil, Nil, Nil) => result
        case (h::t, _, _) => joinHelper(t, l2, l3, h :: result)
        case (Nil, h2::t2, _) => joinHelper(Nil, t2, l3, h2 :: result)
        case (Nil, Nil, h3::t3) => joinHelper(Nil, Nil, t3, h3 :: result)
      }
    }
    reverseList(joinHelper(l1, l2, l3, Nil))



  }

  def main(args: Array[String]): Unit = {
    println(find(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), "index0168"))
    println(find(List("123" ,"1234", "12345"), "123"))
    println(find(List("Example1", "Example2"), ""))

    println(findTail(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), "index0168"))
    println(findTail(List("123" ,"1234", "12345"), "123"))
    println(findTail(List("Example1", "Example2"), ""))



    println(findList(List("123" ,"1234", "12345", "321"), List("123", "321")))
    println(findListTail(List("123" ,"1234", "12345", "321"), List("123", "321")))


    //zadanie 2
    println(joinLists(List(1,2,3), List(4,5), List(6)))
    print(joinListsTail(List(1,2,3), List(4,5), List(6)))
  }
}