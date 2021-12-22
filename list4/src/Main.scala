package list4
import scala.annotation.tailrec
object Main {
    //zad1
    def main(args: Array[String]): Unit = {

      println(findMultiple(List("ab", "abc", "bab", "ba"), "ab") == List("ab", "abc", "bab"))
      println(findMultiple(List("ab", "abc", "bab", "ba"), "c") == List("abc"))
      println(findMultiple(Nil, "c") == List())
      println(findMultiple(List("ab", "abc", "bab", ""), "") == List(""))
      println(findMultiple(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
      println(findMultiple(List("a", "ab", "bc", "cd", "db", "ac", "ad"), "a", "c") == List("a", "ab", "bc", "cd", "ac", "ad"))
      println(findMultiple(List("abc", "cab", "cadc", "dcab", "db", "acab", "ad"), "ab", "ca") == List("abc", "cab", "cadc", "dcab", "acab"))
      println(findMultiple(List("abca", "cab", "bb"), "", "ca") == List("abca", "cab"))

      println(findMultipleTail(List("a", "ab", "bc", "cd", "db", "ac", "ad"), "a", "c") == List("a", "ab", "bc", "cd", "ac", "ad"))
      println(findMultipleTail(List("abc", "cab", "cadc", "dcab", "db", "acab", "ad"), "ab", "ca")  == List("abc", "cab", "cadc", "dcab", "acab"))
      println(findMultipleTail(List("abca", "cab", "bb"), "", "ca") == List("abca", "cab"))

      println(joinLists(List(1,2,3), List(4,5,6,7), List(8)) == List(1, 2, 3, 4, 5, 6, 7, 8))
      println(joinLists(Nil, List(4,5,6,7), List(8)) == List(4, 5, 6, 7, 8))
      println(joinLists(List(1,2,3), Nil, List(8)) == List(1, 2, 3, 8))
      println(joinLists(List(1,2,3), List(4,5,6,7), Nil) == List(1, 2, 3, 4, 5, 6, 7))
      println(joinListsTail(List(1,2,3), List(4,5,6,7), List(8)) == List(1, 2, 3, 4, 5, 6, 7, 8))

    }

    def reverseList[A](list: List[A]): List[A] = {
      @tailrec
      def reverseIter(listToReverse: List[A], reversed: List[A]): List[A] = {
        listToReverse match {
          case Nil => reversed
          case hd :: tl => reverseIter(tl, hd :: reversed)
        }
      }
      reverseIter(list, Nil)
    }

    //O(n)
    def stringStartsWith(phrase: String, substring: String): Boolean = {
      (phrase, substring) match {
        case ("", "") => true
        case (_, "") => true
        case ("", _) => false
        case (_, _) =>
          if phrase.head == substring.head then stringStartsWith(phrase.tail, substring.tail)
          else false
      }
    }

    //O(n^2)
    def stringContains(phrase: String, substring: String): Boolean = {
      @tailrec
      def offsetIter(offsetPhrase: String, toFind: String): Boolean = {
        if offsetPhrase != "" then
          if stringStartsWith(offsetPhrase, toFind) then true
          else offsetIter(offsetPhrase.tail, toFind)
        else false
      }
      if phrase.length < substring.length then false
      else
        (phrase, substring) match {
          case ("", "") => true
          case (_, "") => false
          case _ => offsetIter(phrase, substring)
        }
    }

    //O(k*n^2)
    def find(list: List[String], elem: String): List[String] = {
      list match {
        case h :: t => if stringContains(h, elem) then h :: find(t, elem)
        else find(t, elem)
        case Nil => Nil
      }
    }

    //multiple searching
    //O(m*n^2)
    def stringContainsMultiple(phrase: String, substrings: Seq[String]): Boolean = {
      @tailrec
      def seqIter(sequence: Seq[String]): Boolean = {
        if sequence != Nil then
          if phrase.length < sequence.head.length then seqIter(sequence.tail)
          else if sequence.head == "" && phrase == "" then true
          else if sequence.head == "" && phrase != "" || !offsetIter(phrase, sequence.head) then seqIter(sequence.tail)
          else true
        else false
      }

      @tailrec
      def offsetIter(offsetPhrase: String, toFind: String): Boolean = {
        if offsetPhrase != "" then
          if stringStartsWith(offsetPhrase, toFind) then true
          else offsetIter(offsetPhrase.tail, toFind)
        else false
      }
      seqIter(substrings)
    }

    //O(k*m*n^2)
    //O(k +0.5k^2)
    def findMultiple(list: List[String], elems: String*): List[String] = {
      list match {
        case h :: t =>
          if stringContainsMultiple(h, elems) then h :: findMultiple(t, elems: _*)
          else findMultiple(t, elems: _*)
        case Nil => Nil
      }
    }

    //O(k*m*n^2)
    //O(1)
    def findMultipleTail(list: List[String], elems: String*): List[String] = {
      @tailrec
      def helper(chosen: List[String], toCheck: List[String]): List[String] = {
        toCheck match {
          case h :: t =>
            if stringContainsMultiple(h, elems) then helper(h :: chosen, t)
            else helper(chosen, t)
          case Nil => chosen
        }
      }
      reverseList(helper(Nil, list))
    }

    //zad2

    def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
      (list1, list2) match {
        case (h :: t, _) => h :: joinLists(t, list2, list3)
        case (Nil, h :: t) => h :: joinLists(Nil, t, list3)
        case (Nil, Nil) => list3
      }
    }

    def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
      @tailrec
      def joinListsIter(joined: List[A], listToJoin: List[A]): List[A] = {
        listToJoin match {
          case h :: t => joinListsIter(h :: joined, t)
          case Nil => joined
        }
      }

      reverseList(joinListsIter(joinListsIter(joinListsIter(Nil, list1), list2), list3))
    }
  }

  //złożoność obliczeniowa joinLists O(n + m + 1), gdzie n -l. elementów list1, m - l. elementów list2
  //złożoność pamięciowa joinLists O(0.5n^2 +n +0.5m^2 + m), gdzie n -l. elementów list1, m - l. elementów list2
  //złożoność obliczeniowa  joinListsTail O(2n + 2m + 2k), gdzie n -l. elementów list1, m - l. elementów list2, k - l. elementów list3
  //złożoność pamięciowa joinListsTail O(1)
