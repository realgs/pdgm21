object lab_lista4 {

  //obliczeniowa - O(n)
  //pamięciowa - O(1)
  def reverseList[A](list: List[A]): List[A] =
  {
    def reverseListHelper[A](list: List[A], result: List[A]): List[A] = {
      if list == Nil then result
      else reverseListHelper(list.tail, list.head :: result)
    }

    reverseListHelper(list, Nil)
  }

  //obliczeniowa - O((m-n)*n)
  //pamięciowa - O(1)
  def contains(phrase: String, pattern: String): Boolean =
  {
    val phrase_len = phrase.length
    val pattern_len = pattern.length

    def containsHelper(phrase: String, pattern: String, phrase_pos: Int, pattern_pos: Int): Boolean =
    {
      if phrase_pos > phrase_len - pattern_len then false
      else if pattern_pos == pattern_len then true
      else if phrase.charAt(phrase_pos + pattern_pos) == pattern.charAt(pattern_pos) then containsHelper(phrase, pattern, phrase_pos, pattern_pos + 1)
      else containsHelper(phrase, pattern, phrase_pos + 1, 0)
    }
    containsHelper(phrase, pattern, 0, 0)
  }

  //zadanie 1

  //pojedynczy wzór, rek. zwykła
  //obliczeniowa - O(n)*O(contains)
  //pamięciowa - O(((1+n)/2)*n), n - długość phraseList
  def findPatternNonTail(phraseList: List[String], pattern: String): List[String] =
  {
    if pattern == "" then Nil
    else
      phraseList match {
        case Nil => Nil
        case h :: t => if contains(h, pattern) then h :: findPatternNonTail(t, pattern) else findPatternNonTail(t, pattern)
      }
  }

  //pojedynczy wzór, rek. ogonowa
  //obliczeniowa - O(n)*O(contains) + O(reverse)
  //pamięciowa - O(1)
  def findPatternTail(phraseList: List[String], pattern: String): List[String] =
  {
    def findPatternTailHelper(phraseList: List[String], pattern: String, result: List[String]): List[String] =
    {
      phraseList match {
        case Nil => reverseList(result)
        case h :: t => if contains(h, pattern) then findPatternTailHelper(t, pattern, h :: result) else findPatternTailHelper(t, pattern, result)
      }
    }
    if pattern == "" then Nil
    else findPatternTailHelper(phraseList, pattern, Nil)
  }

  //obliczeniowa - O(n) * O(contains)
  def containsSubstring(phrase: String, patternsList: List[String]): Boolean =
  {
    patternsList match {
      case Nil => false
      case h :: t => if h != "" && contains(phrase, h) then true else containsSubstring(phrase, t)
    }
  }

  //lista wzorów, rek. zwykła
  //obliczeniowa - O(n)*O(containsSubstring), n - długość phraseList
  //pamięciowa - O(((1+n)/2)*n), n - długość phraseList
  def findPatternsNonTail(phraseList: List[String], patternsList: List[String]): List[String] =
  {
    (phraseList, patternsList) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1 :: t1, _) => if containsSubstring(h1, patternsList) then h1 :: findPatternsNonTail(t1, patternsList)
      else findPatternsNonTail(t1, patternsList)
    }
  }

  //lista wzorów, rek. ogonowa
  //obliczeniowa - O(n)*O(containsSubstring) + O(reverse), n - długość phraseList
  //pamięciowa - O(1)
  def findPatternsTail(phraseList: List[String], patternsList: List[String]): List[String] =
  {
    def findPatternsTailHelper(phraseList: List[String], patternsList: List[String], result: List[String]): List[String] =
      {
        (phraseList, patternsList) match {
          case (Nil, _) => reverseList(result)
          case (_, Nil) => reverseList(result)
          case (h1 :: t1, _) => if containsSubstring(h1, patternsList) then findPatternsTailHelper(t1, patternsList, h1::result)
          else findPatternsTailHelper(t1, patternsList, result)
        }
      }
    findPatternsTailHelper(phraseList, patternsList, Nil)
  }

  //zadanie 2

  //rek. zwykła
  //obliczeniowa - O(2a+b), a - dł. 1 listy, b - dł. 2 listy
  //pamięciowa - O(c + (1 + 2 + ... + b) + (1 + 2 + ... + a))
  def joinListsNonTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
  {
    (list1, list2, list3) match {
      case (Nil, Nil, list3) => list3
      case (Nil, h2 :: t2, list3) => h2 :: joinListsNonTail(Nil, t2, list3)
      case (h1 :: t1, list2, list3) => h1 :: joinListsNonTail(t1, list2, list3)
    }
  }

  //rek. ogonowa
  //obliczeniowa - O(a + b + c) + O(reverse) - a - dł. 1 listy, b - dł. 2 listy, c - dł. 3 listy
  //pamięciowa - O(1)
  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
  {
    def joinListsTailHelper[A](list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] =
    {
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => reverseList(result)
        case (Nil, Nil, h3 :: t3) => joinListsTailHelper(Nil, Nil, t3, h3 :: result)
        case (Nil, h2 :: t2, list3) => joinListsTailHelper(Nil, t2, list3, h2 :: result)
        case (h1 :: t1, list2, list3) => joinListsTailHelper(t1, list2, list3, h1 :: result)
      }
    }
    joinListsTailHelper(list1, list2, list3, Nil)
  }


  def main(args: Array[String]): Unit = {

    println(findPatternNonTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findPatternTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))

    println(findPatternNonTail(List("Ala"), "") == List())
    println(findPatternTail(List("Ala"), "") == List())

    println(findPatternNonTail(List("motyw", "lokomotywa", "lokomocja"), "motyw") == List("motyw", "lokomotywa"))
    println(findPatternTail(List("motyw", "lokomotywa", "lokomocja"), "motyw") == List("motyw", "lokomotywa"))

    println(findPatternNonTail(List(), "pusto") == List())
    println(findPatternTail(List(), "pusto") == List())

    println(findPatternsNonTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("ii")) == List("iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"))
    println(findPatternsTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("ii")) == List("iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"))

    println(findPatternsNonTail(List("Stal", "Gorzów", "mistrzem", "Polski"), List("Gorzów", "mistrz")) == List("Gorzów", "mistrzem"))
    println(findPatternsTail(List("Stal", "Gorzów", "mistrzem", "Polski"), List("Gorzów", "mistrz")) == List("Gorzów", "mistrzem"))

    println(findPatternsNonTail(List("Ala", "ma", "kota", "a", "kot", "ma", "kuwetę"), List("k", "ę")) == List("kota", "kot", "kuwetę"))
    println(findPatternsTail(List("Ala", "ma", "kota", "a", "kot", "ma", "kuwetę"), List("k", "ę")) == List("kota", "kot", "kuwetę"))

    println(findPatternsNonTail(List("jeden", "dwa", "trzy"), List()) == List())
    println(findPatternsTail(List("jeden", "dwa", "trzy"), List()) == List())

    println()

    println(joinListsNonTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))

    println(joinListsNonTail(List(1, 2, 3), List(), List(5, 6, 7, 8)) == List(1, 2, 3, 5, 6, 7, 8))
    println(joinListsTail(List(1, 2, 3), List(), List(5, 6, 7, 8)) == List(1, 2, 3, 5, 6, 7, 8))

    println(joinListsNonTail(List(), List('a', 'b'), List('c')) == List('a', 'b', 'c'))
    println(joinListsTail(List(), List('a', 'b'), List('c')) == List('a', 'b', 'c'))

    println(joinListsNonTail(List("Ala", "ma", "kota"), List("a", "kot", "kuwetę"), List()) == List("Ala", "ma", "kota", "a", "kot", "kuwetę"))
    println(joinListsTail(List("Ala", "ma", "kota"), List("a", "kot", "kuwetę"), List()) == List("Ala", "ma", "kota", "a", "kot", "kuwetę"))


  }

}
