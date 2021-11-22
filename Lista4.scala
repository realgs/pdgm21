import annotation.tailrec

object Lista4 {
  def contains(string: String, substring: String): Boolean = {
    def helper(string: String, phrase: String): Boolean = {
      (string, phrase) match
        case (_, "") => true
        case ("", _) => false
        case (_, _)  => if (string.head == phrase.head) then helper(string.tail, phrase.tail)
                        else if (string.head == substring.head) then helper(string, substring)
                        else helper(string.tail, substring)
    }
    helper(string, substring)
  }

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
  def find(list: List[String], element: String): List[String] = {
    (list, element) match
      case (Nil, _) => Nil
      case (_, "") => Nil
      case (head :: tail, _) => if contains(head, element) then head :: find(tail, element)
                          else find(tail, element)

  }
  def findTail(list: List[String], element: String) = {
    @tailrec
    def helper(list: List[String], element: String, resultList: List[String]): List[String] = {
      (list, element) match
        case (Nil, _) => resultList
        case (_, "")  => Nil
        case (head :: tail, _) => if contains(head, element) then helper(tail, element, head :: resultList)
                                  else helper(tail, element, resultList)
    }
    reverse(helper(list, element, Nil))
  }
//zadanie 1, N stringow
  def findN(list: List[String], elements: List[String]): List[String] = {
    def checkString(string: String, elements: List[String]): Boolean = {
      elements match
        case Nil => false
        case "" :: _ => false
        case head :: tail =>  if contains(string, head) then true
                              else checkString(string, tail)
    }
    (list, elements) match
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (head :: tail, _) =>  if checkString(head, elements) then head :: findN(tail, elements)
                            else findN(tail, elements)
  }
  def findNTail(list: List[String], elements: List[String]): List[String] = {
    def checkString(string: String, elements: List[String]): Boolean = {
      elements match
        case Nil => false
        case "" :: _ => false
        case head :: tail =>  if contains(string, head) then true
                              else checkString(string, tail)
    }
    @tailrec
    def helper(strings: List[String], elements: List[String], resultList: List[String]): List[String] = {
      (strings, elements) match
        case (Nil, _) => resultList
        case (_, Nil) => resultList
        case (h :: t, _) => if checkString(h, elements) then helper(t, elements, h :: resultList)
                            else helper(t, elements, resultList)
    }
    reverse(helper(list, elements, Nil))
  }

  //zadanie 2
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match
      case (Nil, Nil, _) => list3
      case (h1 :: t1, _, _) => h1 :: joinLists(t1, list2, list3)
      case (Nil, h2 :: t2, _) => h2 :: joinLists(list1, t2, list3)
  }

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
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),""))
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),""))
    println(find(List("ala", "alla", "ala ma kota", "ma kota", "Ala"), "ala"))
    println(findTail(List("ala", "alla", "kota ma _ala_", "ma kota", "Ala"), "ala"))
    println(find(List(), "ala"))
    println(findTail(List(), "ala"))
    println()

    println(findN(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168", "index01692")))
    println(findNTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168", "index01692")))
    println(findN(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("")))
    println(findNTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("")))
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
