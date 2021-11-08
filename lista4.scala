import scala.annotation.tailrec

case object lista4{

  // returns size of a string
  val myLength: String => Int = str => {
    // time complexity: n, where n is string length
    @tailrec
    def myLengthHelper(str:String, sum:Int):Int =
      if(str=="") then sum else myLengthHelper(str.tail, sum+1)
    myLengthHelper(str, 0)
  }

  // returns reversed list
  def myReverse[A](list:List[A]):List[A] = {
    // time complexity: n, where n is list length
    @tailrec
    def myReverseHelper(list:List[A], result: List[A]):List[A] = {
      if list.isEmpty then result
      else myReverseHelper(list.tail, list.head::result)
    }
    myReverseHelper(list, List())
  }
  
  // checks if a phrase contains a pattern
  @ tailrec
  def containsString(phrase: String, pattern :String):Boolean = {
    // complexity: n where n is min(phrase.length, pattern.length)
    (phrase, pattern) match {
      case ("","") => true
      case ("",_) => false
      case (_,"") => true
      case (_, _) => if phrase.head == pattern.head then containsString(phrase.tail, pattern.tail)
        else containsString(phrase.tail, pattern)
    }
  }

  // checks if a list contains a value (elem)
  @tailrec
  def containsList[A](list:List[A], elem:A):Boolean = {
    // time complexity of n where n is list length.
    if(list.isEmpty) then false
    else if(list.head==elem) then true
    else (containsList(list.tail, elem))
  }

  // searches for just one pattern in a list of strings, no tail recursion
  val findNoTail: (List[String], String) => List[String ] = (list, elem) => {
    // time complexity: n * complexity of containsString() = n * m,
    //   where n is length of the list, m is ?
    if(list.isEmpty) list
    else if(containsString(list.head, elem)) list.head::findNoTail(list.tail, elem)
    else findNoTail(list.tail, elem)
  }

  // makes use of tail recursion, searches for just one pattern in a list of strings
  val findTail: (List[String], String) => List[String ] = (list, elem) => {
    // time complexity: n * complexity of containsString() = n * m,
    //   where n is length of the list, m is ?
    @tailrec
    def findHelper (list: List[String], elem: String, results:List[String]):List[String] = {
      if(list.isEmpty) myReverse(results)
      else if(containsString(list.head, elem)) then
        findHelper(list.tail, elem, list.head::results)
      else findHelper(list.tail, elem, results)
    }
    findHelper(list, elem, List())  // gets called n times
  }

  // makes use of tail recursion, searches for multiple patterns in the list of strings
  val findMultiple: (List[String], List[String]) => List[String] = (list, patterns) => {
    // time complexity:
    // complexity of myReverse() + n * m * (complexity of containsString() + complexity of containsList())
    //   == n + n * m * p,
    //   where: n - size of list of texts, m - size of the list of patterns, p - size of phrases and patterns ?
    @tailrec
    def findHelper (list: List[String], results:List[String], currentPattern:List[String]):List[String] = {

      if(list.isEmpty) then myReverse(results) // complexity of list.length

      else if (currentPattern.isEmpty) then findHelper(list.tail, results, patterns)
      else if (!containsString(list.head, currentPattern.head)) findHelper(list, results, currentPattern.tail)
      else
        findHelper(
          list.tail,
          if(!containsList(results, currentPattern.head)) then list.head::results else results,
          patterns)
    }
    findHelper(list, List(), patterns)  // gets called n*m times
    
  }

  // joinLists() concatenates three lists
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    // time complexity: 2*(k+l+m),
    //   where k is list1 length, l is list2 length, m is list3 length.
    // helper method joinTwoLists() adds reversed list2 in front of list1
    @tailrec
    def joinTwoLists[A](list1: List[A], list2: List[A]):List[A] = {
      if(list2.isEmpty) then list1
      else joinTwoLists(list2.head::list1, list2.tail)
    }
    myReverse(joinTwoLists(joinTwoLists(myReverse(list1), list2), list3))
  }

  def main(args: Array[String]) = {

    println("TESTS\n")

    println("search for a pattern in a text list, no tail recursion:")
    println(findNoTail( List("index0169","iindex0168202","iindex0168211",
      "iindex0168210", "iindex0169222", "index0169224"), "index0168")
      == List("iindex0168202","iindex0168211","iindex0168210"))
    println(findNoTail( List("ala", "ma", "kota"), "psa") == List())
    println(findNoTail( List("ala", "ma", "kota"), "ma") == List("ma"))
    println(findNoTail( List(), "psa") == List())

    println("search for a pattern in a text list, with tail recursion:")
    println(findTail(List("index0169","iindex0168202","iindex0168211",
      "iindex0168210", "iindex0169222", "iindex0169224"), "index0168")
      == List("iindex0168202","iindex0168211","iindex0168210"))
    println(findTail( List("ala", "ma", "kota"), "ma") == List("ma"))
    println(findTail( List("ala", "ma", "kota"), "psa") == List())
    println(findTail( List(), "psa") == List())

    println("search for multiple patterns in a text list, with tail recursion:")
    println(findMultiple( List("000", "001", "010", "011", "100", "101", "110", "111"),
      List("11", "00","000", "111")) == List("000", "001", "011", "100", "110", "111"))
    println(findMultiple( List("000", "001", "010", "011", "100", "101", "110", "111"),
      List("101", "010")) == List("010", "101"))
    println(findMultiple( List("000", "001", "010"), List()) == List())
    println(findMultiple(List(), List("1", "0")) == List())

    println("join three lists:")
    println(joinLists(List(1,2,3),List(4,5,6),List(7,8,9)) == List(1,2,3,4,5,6,7,8,9))
    println(joinLists(List(),List(4,5,6),List(7,8,9))==List(4,5,6,7,8,9))
    println(joinLists(List('a'),List('b', 'c'),List())==List('a', 'b', 'c'))
    println(joinLists(List(),List(),List()) == List())

  }

}

