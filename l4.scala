object l4 {
  // task 1

  // O(n)
  def stringLength(str: String): Int =
    if str == "" then 0 else 1 + stringLength(str.tail)

  // O(n)
  def beginsWithPhrase(phrase: String, word: String): Boolean =
    (phrase, word) match
      case ("", w) => true
      case (p, "") => false
      case (p, w) => if p.head == w.head then beginsWithPhrase(p.tail, w.tail) else false

  // O(n^2)
  def containsPhrase(phrase: String, word: String): Boolean =
    val phraseLength = stringLength(phrase)
    def containsPhraseIn(wordIn: String): Boolean =
      val wordLength = stringLength(wordIn)
      if wordLength >= phraseLength then
        (if beginsWithPhrase(phrase, word) then true else containsPhrase(phrase, word.tail))
      else false
    containsPhraseIn(word)

  // O(n) względem długości pierwszej listy
  def appendLists[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match
      case (h1 :: t1, list2) => h1 :: appendLists(t1, list2)
      case (Nil, list2) => list2

  // O(n^3)
  def find(list: List[String], phrase: String): List[String] =
    list match
      case Nil => Nil
      case h :: t => if containsPhrase(phrase, h) then h :: find(t, phrase) else find(t, phrase)

  find (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168")

  // O(n^3)
  def findTail(list: List[String], phrase: String): List[String] =
    def findTailIn(listIn: List[String], resultList: List[String]): List[String] =
      listIn match
        case Nil => resultList
        case h :: t => if containsPhrase(phrase, h) then findTailIn(t, h :: resultList) else findTailIn(t, resultList)
    findTailIn(list, List())

  findTail (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168")

  // O(n^4)
  def findN(wordsList: List[String], phrasesList: List[String]): List[String] =
    phrasesList match
      case Nil => Nil
      case h :: t => appendLists(find(wordsList, h), findN(wordsList, t))

  findN (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "index01692"))

  // O(n^4)
  def findNTail(wordsList: List[String], phrasesList: List[String]): List[String] =
    def findNTailIn(phrasesListIn: List[String], resultList: List[String]): List[String] =
      phrasesListIn match
        case Nil => resultList
        case h :: t => findNTailIn(t, appendLists[String](findTail(wordsList, h), resultList))
    findNTailIn(phrasesList, List())

  findNTail (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "index01692"))


  // task 2
  
  // O(n + m), gdzie n to długość 1 listy, a m drugiej
  // złożoność liniowa względem sumy długości wszystkich list
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (h1 :: t1, list2, list3) => h1 :: joinLists(t1, list2, list3)
      case (Nil, h2 :: t2, list3) => h2 :: joinLists(Nil, t2, list3)
      case (Nil, Nil, list3) => list3

  joinLists(List(5, 4, 3, 2), List(1, 0), List(9))
  joinLists(List("Ala", "ma"), List(), List("kota"))
  joinLists(Nil, Nil, Nil)

  // O(n)
  def reverseList[A](list: List[A]): List[A] =
    def reverseListIn[A](innerList: List[A], resultList: List[A]): List[A] =
      if innerList == Nil then resultList else reverseListIn(innerList.tail, innerList.head :: resultList)
    reverseListIn(list, List())

  // O(n + m + k), gdzie n to długość pierwszej listy, m drugiej, a k trzeciej
  // złożoność liniowa względem sumy długości wszystkich list
  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    def joinListsTailIn[A](list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] =
      (list1, list2, list3) match
        case (h1 :: t1, list2, list3) => joinListsTailIn(t1, list2, list3, h1 :: result)
        case (Nil, h2 :: t2, list3) =>  joinListsTailIn(Nil, t2, list3, h2 :: result)
        case (Nil, Nil, h3 :: t3) => joinListsTailIn(Nil, Nil, t3, h3 :: result)
        case (Nil, Nil, Nil) => reverseList(result)
    joinListsTailIn(list1, list2, list3, Nil)

  joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9))
  joinListsTail(List("Ala", "ma"), List(), List("kota"))
  joinListsTail(Nil, Nil, Nil)


  //main
  def main () = {
    println("find(List(\"index0169\", \"iindex0168202\", \"iindex0168211\", \"iindex0168210\", \"iindex0169222\", \"index0169224\"), \"index0168\"): "
      + find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println("find(List(\"Ala\", \"ma\", \"kota\", \"a\", \"kot\", \"ma\", \"Ale\"), \"kot\"): "
      + find(List("Ala", "ma", "kota", "a", "kot", "ma", "Ale"), "kot"))
    println("find(List(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), \"1\"): "
      + find(List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), "1"))

    println("findTail(List(\"index0169\", \"iindex0168202\", \"iindex0168211\", \"iindex0168210\", \"iindex0169222\", \"index0169224\"), \"index0168\"): "
      + findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println("findTail(List(\"Ala\", \"ma\", \"kota\", \"a\", \"kot\", \"ma\", \"Ale\"), \"kot\"): "
      + findTail(List("Ala", "ma", "kota", "a", "kot", "ma", "Ale"), "kot"))
    println("find(List(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), \"1\"): "
      + findTail(List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), "1"))

    println("findN (List(\"index0169\", \"iindex0168202\", \"iindex0168211\", \"iindex0168210\", \"iindex0169222\", \"index0169224\"), List(\"index0168\", \"index01692\")): "
      + findN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "index01692")))
    println("findN(List(\"Ala\", \"ma\", \"kota\", \"a\", \"kot\", \"ma\", \"Ale\"), List(\"kot\", \"Al\")): "
      + findN(List("Ala", "ma", "kota", "a", "kot", "ma", "Ale"), List("kot", "Al")))
    println("findN(List(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), List(\"1\", \"12\")): "
      + findN(List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), List("1", "12")))

    println("findNTail(List(\"index0169\", \"iindex0168202\", \"iindex0168211\", \"iindex0168210\", \"iindex0169222\", \"index0169224\"), List(\"index0168\", \"index01692\")): "
      + findNTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "index01692")))
    println("findNTail(List(\"Ala\", \"ma\", \"kota\", \"a\", \"kot\", \"ma\", \"Ale\"), List(\"kot\", \"Al\")): "
      + findNTail(List("Ala", "ma", "kota", "a", "kot", "ma", "Ale"), List("kot", "Al")))
    println("findNTail(List(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), List(\"1\", \"12\")): "
      + findNTail(List("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), List("1", "12")))


    println("joinLists(List(5, 4, 3, 2), List(1, 0), List(9)): " + joinLists(List(5, 4, 3, 2), List(1, 0), List(9)))
    println("joinLists(List(\"Ala\", \"ma\"), List(), List(\"kota\")): " + joinLists(List("Ala", "ma"), List(), List("kota")))
    println("joinLists(Nil, Nil, Nil): " + joinLists(Nil, Nil, Nil))

    println("joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)): " + joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)))
    println("joinListsTail(List(\"Ala\", \"ma\"), List(), List(\"kota\")): " + joinListsTail(List("Ala", "ma"), List(), List("kota")))
    println("joinListsTail(Nil, Nil, Nil): " + joinListsTail(Nil, Nil, Nil))
  }
}
