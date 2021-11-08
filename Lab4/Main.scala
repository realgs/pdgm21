import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) = {

    //exercise 1 - test
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find(List("index0169", "iin", "iindex0168211"), "index0168") == List("iindex0168211"))
    println(find(List(), "22") == Nil)

    println(findTailRec(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findTailRec(List("index0169", "iin", "iindex0168211"), "index0168") == List("iindex0168211"))
    println(findTailRec(List(), "22") == Nil)

    println(findN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "asdfabcHdsf", "iindex0169222", "index0169224"), List("index0168", "016", "abc")) == List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "asdfabcHdsf", "iindex0169222", "index0169224"))
    println(findN(List("index0169", "iin", "iindex0168211"), List("index0168")) == List("iindex0168211"))
    println(findN(List("index0169", "iin", "iindex0168211"), Nil) == Nil)
    println(findN(List(), List("22", "sdf")) == Nil)

    println(findTailRecN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findTailRecN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "asdfabcHdsf", "iindex0169222", "index0169224"), List("index0168", "016", "abc")) == List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "asdfabcHdsf", "iindex0169222", "index0169224"))
    println(findTailRecN(List("index0169", "iin", "iindex0168211"), List("index0168")) == List("iindex0168211"))
    println(findTailRecN(List("index0169", "iin", "iindex0168211"), Nil) == Nil)
    println(findTailRecN(List(), List("22", "sdf")) == Nil)
    println()

    //exercise 2 - test
    println(join(Nil, Nil, Nil) == Nil)
    println(join(Nil, List(1, 2, 3), Nil) == List(1, 2, 3))
    println(join(List('a', 'b'), List('c', 'd'), List('e', 'f')) == List('a', 'b', 'c', 'd', 'e', 'f'))
    println(join(List(1, 2), List(3, 4, 5), List(6)) == List(1, 2, 3, 4, 5, 6))

    println(joinTailRec(Nil, Nil, Nil) == Nil)
    println(joinTailRec(Nil, List(1, 2, 3), Nil) == List(1, 2, 3))
    println(joinTailRec(List('a', 'b'), List('c', 'd'), List('e', 'f')) == List('a', 'b', 'c', 'd', 'e', 'f'))
    println(joinTailRec(List(1, 2), List(3, 4, 5), List(6)) == List(1, 2, 3, 4, 5, 6))
  }

  //time complexity: O((k + 1)n - n^2), k >= n     //(k + 1 - n) * n
  //space complexity: k + n
  //k = length of main phrase; n = length of searched phrase
  def checkIfContains(mainPhrase: String, phrase: String) =

    //int size = 4 bytes; char size = 1 byte; boolean size: 1 bit - not significant
    //space complexity: 20 bytes; O(1)       //4 + 4 + 8 + 4
    def iterateList(pos: Int): Boolean =

      if pos < (phrase.length - 1) then false
      else if check(pos, phrase.length - 1) then true
      else iterateList(pos - 1)

    //space complexity: 18 bytes; O(1)  //8 + (2 * 1 + 2 * 4)
    def check(mainPos: Int, position: Int): Boolean =

      if position < 0 then true
      else (mainPhrase.charAt(mainPos) == phrase.charAt(position)) && check(mainPos - 1, position - 1)

    iterateList(mainPhrase.length - 1)

  def checkIfAnyContains(mainPhrase: String, phrases: List[String]): Boolean =

    if phrases == Nil then false
    else if checkIfContains(mainPhrase, phrases.head) then true
    else checkIfAnyContains(mainPhrase, phrases.tail)

  //time complexity: O(n*((k + 1)m - m^2))
  //space complexity: n^2k^2/2 - 5/2kn + 2mn + m      //(nk + m) + n(k + m) + (nk + ((nk-1)+1)(nk-1) / 2 + nm)
  //n = number of phrases, k = length of the shortest phrase from the list, m = length of searched phrase
  def find(phrases: List[String], phrase: String): List[String] =

    if phrases == Nil then Nil
    else if checkIfContains(phrases.head, phrase) then phrases.head :: find(phrases.tail, phrase)
    else find(phrases.tail, phrase)

  //extended verion of find() method
  def findN(phrases: List[String], searchedPhrases: List[String]): List[String] =

    if phrases == Nil then Nil
    else if checkIfAnyContains(phrases.head, searchedPhrases) then phrases.head :: findN(phrases.tail, searchedPhrases)
    else findN(phrases.tail, searchedPhrases)

  //time complexity: O(n*((k + 1)m - m^2))
  //space complexity: 9nk + k + 2m      //(nk + m) + nk + (nk + (k + m) + nk) + 5nk //tail recursion don't need to add new stack frame; it replaces the previous one
  //n = number of phrases, k = length of the shortest phrase from the list, m = length of searched phrase
  def findTailRec(phrases: List[String], phrase: String) =

    @tailrec
    def findHelper(list: List[String], resultList: List[String]): List[String] =

      if list == Nil then resultList
      else if checkIfContains(list.head, phrase) then findHelper(list.tail, list.head :: resultList)
      else findHelper(list.tail, resultList)
    reverse(findHelper(phrases, List()))

  //extended verion of findTailRec() method
  def findTailRecN(phrases: List[String], searchedPhrases: List[String]) =

    def findHelper(phrasesList: List[String], resultList: List[String]): List[String] =

      if phrasesList == Nil then resultList
      else if checkIfAnyContains(phrasesList.head, searchedPhrases) then findHelper(phrasesList.tail, phrasesList.head :: resultList)
      else findHelper(phrasesList.tail, resultList)
    reverse(findHelper(phrases, List()))

  //time complexity: O(n)
  //space complexity: n^2 + 8n; O(n^2)       //3n + (n + n + (((n-1) + 1) / 2)(n - 1) + n + n + (((n-1) + 1) / 2)(n - 1) + n + n)
  //n = length of the shortest list
  def join[T](list1: List[T], list2: List[T], list3: List[T]): List[T] =

    (list1, list2, list3) match
        case(Nil, Nil, _) => list3
        case(Nil, head::tail, _) => head :: join(Nil, tail, list3)
        case(head::tail, _, _) => head :: join(tail, list2, list3)

  //time complexity: O(n)
  //space complexity: 25n; O(n)      //3n + 3n + (n + 3n) 5*3n
  //n = length of the shortest list
  def joinTailRec[T](list1: List[T], list2: List[T], list3: List[T]) =

    @tailrec
    def joinHelper[T](list1: List[T], list2: List[T], list3: List[T], resultList: List[T]): List[T] =

      (list1, list2, list3) match
        case(Nil, Nil, Nil) => resultList
        case(Nil, Nil, head::tail) => joinHelper(Nil, Nil, tail, head::resultList)
        case(Nil, head::tail, _) => joinHelper(Nil, tail, list3, head::resultList)
        case(head::tail, _, _) => joinHelper(tail, list2, list3, head::resultList)
    reverse(joinHelper(list1, list2, list3, List()))

  //time complexity: O(n)
  //space complexity: 5n; O(n)       //n + n + 2n + n
  //n = length of the list
  def reverse[T](list: List[T]): List[T] =

    def reverseHelper[T](list: List[T], revList: List[T]): List[T] =

      if list == Nil then revList
      else reverseHelper(list.tail, list.head :: revList)
    reverseHelper(list, List())

}
