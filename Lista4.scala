import annotation.tailrec

object Lista4 {
  // time: O(n * m)
  // space: O(n + m)
  def contains(string: String, substring: String): Boolean = {
    def helper(string: String, phrase: String): Boolean = {
      (string, phrase) match
        case (_, "") => true
        case ("", _) => false
        case (_, _) =>  if (string.head == phrase.head) then helper(string.tail, phrase.tail)
                        else if (string.head == substring.head) then helper(string, substring)
                        else helper(string.tail, substring)
    }
    helper(string, substring)
  }

  // time: O(n)
  // space: O(n)
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def helper(list: List[A], result: List[A]): List[A] = {
      list match
        case Nil => result
        case h :: t => helper(t, h :: result)
    }
    helper(list, Nil)
  }

  //zadanie 1, pojedynczny string
  // time: O(n * k), n - dlugosc listy, k - zlozonosc funkcji contains
  // space: O(n^2)
  def find(list: List[String], element: String): List[String] = {
    if list == Nil then Nil
    else if contains(list.head, element) then list.head :: find(list.tail, element)
    else find(list.tail, element)
  }
  // time: O(n * k), n - dlugosc listy, k - zlozonosc funkcji contains
  // space: O(n)
  def findTail(list: List[String], element: String) = {
    @tailrec
    def helper(list: List[String], element: String, resultList: List[String]): List[String] = {
      if list == Nil then resultList
      else if contains(list.head, element) then helper(list.tail, element, list.head :: resultList)
      else helper(list.tail, element, resultList)
    }
    reverse(helper(list, element, Nil))
  }
  //zadanie 1, N stringow
  // time: O(n * m * k), n - dlugosc listy, m - dlugosc listy fraz, k - zlozonosc funkcji contains
  def findN(list: List[String], elements: List[String]): List[String] = {
    def checkString(string: String, elements: List[String]): Boolean = {
      if elements == Nil then false
      else if contains(string, elements.head) then true
      else checkString(string, elements.tail)
    }
    list match
      case Nil => Nil
      case head :: tail =>  if checkString(head, elements) then head :: findN(tail, elements)
                            else findN(tail, elements)
  }
  // time: O(n * m * k), n - dlugosc listy, m - dlugosc listy fraz, k - zlozonosc funkcji contains
  def findNTail(list: List[String], elements: List[String]): List[String] = {
    def checkString(string: String, elements: List[String]): Boolean = {
      if elements == Nil then false
      else if contains(string, elements.head) then true
      else checkString(string, elements.tail)
    }
    @tailrec
    def helper(strings: List[String], elements: List[String], resultList: List[String]): List[String] = {
      (strings, elements) match
        case (Nil, _) => resultList
        case (_, Nil) => strings
        case (h :: t, _) => if checkString(h, elements) then helper(t, elements, h :: resultList)
                            else helper(t, elements, resultList)
    }
    reverse(helper(list, elements, Nil))
  }

  //zadanie 2
  // time: O(n), dlugosc list 1 oraz 2
  // space: O(n^2)
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match
      case (Nil, Nil, _) => list3
      case (h1 :: t1, _, _) => h1 :: joinLists(t1, list2, list3)
      case (Nil, h2 :: t2, _) => h2 :: joinLists(list1, t2, list3)
  }
  // time: O(n), dlugosc wszystkich list
  // space: O(n)
  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]) = {
    @tailrec
    def helper(list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] = {
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => result
        case (h1 :: t1, _, _) => helper(t1, list2, list3, h1 :: result)
        case (Nil, h2 :: t2, _) => helper(list1, t2, list3, h2 :: result)
        case (Nil, Nil, h3 :: t3) => helper(list1, list2, t3, h3 :: result)
    }
    reverse(helper(list1, list2, list3, Nil))
  }

  def main(args: Array[String]): Unit = {
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168"))
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168"))
    println(find(List("ala", "alla", "ala ma kota", "ma kota", "Ala"), "ala"))
    println(findTail(List("ala", "alla", "kota ma _ala_", "ma kota", "Ala"), "ala"))
    println(find(List(), "ala"))
    println(findTail(List(), "ala"))
    println()

    println(findN(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168", "index01692")))
    println(findNTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168", "index01692")))
    println(findN(List("waga", "kawa", "ala", "fala"), List("wa", "fa")))
    println(findNTail(List("waga", "kawa", "ala", "fala"), List("wa", "fa")))
    println(findN(List(), List("ala", "kawa")))
    println(findNTail(List(), List("ala", "kawa")))
    println()

    println(joinLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    println(joinLists(List(), List(), List()))
    println(joinLists(List(), List(), List(1, 1, 2, 2, 3, 4)))
  }
}
