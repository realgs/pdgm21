object list4 {

  def reverse[A](xs: List[A]): List[A] =
    def reverseRec[A](xs: List[A], x: List[A]): List[A] =
      if xs == List() then x
      else reverseRec(xs.tail, xs.head :: x)

    reverseRec(xs, List())
  //  Złożoność obliczeniowa - O(n)
  //  Złożoność pamięciowa - O(n)


  //Zadanie 1

  def contains(word: String, phrase: String): Boolean =
    def containsRec(word: String, phrase: String, tempWord: String, tempPhrase: String): Boolean =
      (tempWord, tempPhrase) match
        case (_, "") => true
        case ("", _) => false
        case (_, _) =>
          if tempWord.head == tempPhrase.head then containsRec(word, phrase, tempWord.tail, tempPhrase.tail)
          else containsRec(word.tail, phrase, word.tail, phrase)

    containsRec(word, phrase, word, phrase)

  def find(wordList: List[String], phraseList: List[String]): List[String] =
    def findRec(wordList: List[String], phraseListIn: List[String]): List[String] =
      (wordList, phraseListIn) match
        case (head1 :: tail1, head2 :: tail2) =>
          if (contains(head1, head2)) then head1 :: findRec(tail1, phraseList)
          else if (tail2 != Nil) findRec(wordList, tail2)
          else findRec(tail1, phraseList)
        case (_, _) => Nil

    findRec(wordList, phraseList)

  def findTail(wordList: List[String], phraseList: List[String]): List[String] =
    def findTailRec(wordList: List[String], phraseListIn: List[String], result: List[String]): List[String] =
      (wordList, phraseListIn) match
        case (h1 :: t1, h2 :: t2) =>
          if (contains(h1, h2)) then findTailRec(t1, phraseList, h1 :: result)
          else if (t2 != Nil) findTailRec(wordList, t2, result)
          else findTailRec(t1, phraseList, result)
        case (_, _) => result

    findTailRec(wordList, phraseList, Nil)


  //Zadanie 2


  def joinLists[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] =
    (xs1, xs2, xs3) match
      case (h1 :: t1, _, _) => h1 :: joinLists(t1, xs2, xs3)
      case (Nil, h2 :: t2, _) => h2 :: joinLists(Nil, t2, xs3)
      case (Nil, Nil, h3 :: t3) => h3 :: joinLists(Nil, Nil, t3)
      case (_, _, _) => Nil
  //  Złożoność obliczeniowa - O(n) gdzie n jest równe sumie długości ciągów xs1 xs2 i xs3
  //  Złożoność pamięciowa - O((xs1^2+xs1)/2 +(xs2^2+xs2)/2 +(xs3^2+xs3)/2)

  def joinListsTail[A](xs1: List[A], xs2: List[A], xs3: List[A]): List[A] =
    def joinListsTailRec[A](xs1: List[A], xs2: List[A], xs3: List[A], result: List[A]): List[A] =
      (xs1, xs2, xs3) match
        case (h1 :: t1, _, _) => joinListsTailRec(t1, xs2, xs3, h1 :: result)
        case (Nil, h2 :: t2, _) => joinListsTailRec(Nil, t2, xs3, h2 :: result)
        case (Nil, Nil, h3 :: t3) => joinListsTailRec(Nil, Nil, t3, h3 :: result)
        case (_, _, _) => result

    reverse(joinListsTailRec(xs1, xs2, xs3, List()))
  //  Złożoność obliczeniowa - O(2n) gdzie n jest równe sumie długości ciągów xs1 xs2 i xs3
  //  Złożoność pamięciowa - O(n)

  def main(args: Array[String]) = {
    println(joinLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    println(joinLists(List(1, 2, 3), List(4, 5, 6), Nil) == List(1, 2, 3, 4, 5, 6))
    println(joinLists(List(1, 2, 3), Nil, List(7, 8, 9)) == List(1, 2, 3, 7, 8, 9))
    println(joinLists(List(), List(), List()) == List())

    println(joinListsTail(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) == List(1, 2, 3, 4, 5, 6, 7, 8, 9))
    println(joinListsTail(List(1, 2, 3), List(4, 5, 6), Nil) == List(1, 2, 3, 4, 5, 6))
    println(joinListsTail(List(1, 2, 3), Nil, List(7, 8, 9)) == List(1, 2, 3, 7, 8, 9))
    println(joinListsTail(List(), List(), List()) == List())


    println(find(List("abcba", "abaaac", "xxxsss","abacaca", "aba", "xxxusss"), List("aa", "xxss")))
    println(find(List("abcba"), List("aa", "xxss")))
    println(find(List("abcba", "abaaac", "xxxsss","abacaca", "aba", "xxxusss"), List("xxss")))
    println(find(List("abcba", "abaaac", "xxxsss","abacaca", "aba", "xxxusss"), List()))
    println(find(List(), List("xxss")))


    println(findTail(List("abcba", "abaaac", "xxxsss","abacaca", "aba", "xxxusss"), List("aa", "xxss")))
    println(findTail(List("abcba"), List("aa", "xxss")))
    println(findTail(List("abcba", "abaaac", "xxxsss","abacaca", "aba", "xxxusss"), List("xxss")))
    println(findTail(List("abcba", "abaaac", "xxxsss","abacaca", "aba", "xxxusss"), List()))
    println(findTail(List(), List("xxss")))
  }


}
