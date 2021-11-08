package mb

import scala.annotation.tailrec

object Main {

  //task 1

  def findPattern(listOfTexts: List[String], pattern: String): List[String] = {
    listOfTexts match
      case Nil => List()
      case head :: tail => if containsPattern(head, pattern) then head :: findPattern(tail, pattern) else findPattern(tail, pattern)
  }


  def findPatternTail(listOfTexts: List[String], pattern: String): List[String] = {
    @tailrec
    def findPatternTailInner(listOfTexts: List[String], resultList: List[String]): List[String] =
      listOfTexts match
        case Nil => resultList
        case head :: tail => if containsPattern(head, pattern) then findPatternTailInner(tail, head :: resultList) else findPatternTailInner(tail, resultList)
    reverseList(findPatternTailInner(listOfTexts, List()))
  }


  def findPatterns(listOfTexts: List[String], patterns: List[String]): List[String] = {
      listOfTexts match
        case Nil => List()
        case head :: tail => if containsPatterns(head, patterns) then head :: findPatterns(tail, patterns) else findPatterns(tail, patterns)
  }

 
  def findPatternsTail(listOfTexts: List[String], patterns: List[String]): List[String] = {
    @tailrec
      def findPatternsTailInner(listOfTexts: List[String], resultList: List[String]): List[String] =
        listOfTexts match
          case Nil => resultList
          case head :: tail => if containsPatterns(head, patterns) then findPatternsTailInner(tail, head :: resultList) else findPatternsTailInner(tail, resultList)
      reverseList(findPatternsTailInner(listOfTexts, List()))
  }


  def containsPatterns(investigatedText: String, patterns: List[String]): Boolean = {
    patterns match
      case Nil => false
      case head :: tail => if !containsPattern(investigatedText, head) then containsPatterns(investigatedText, tail) else true
  }

 
  def containsPattern(investigatedText: String, patternText: String): Boolean = {
    if patternText.isEmpty then false else {
      @tailrec
      def containsPatternInner(text: String, pattern: String, textTail: String): Boolean =
        (text, pattern) match
          case (_, "") => true
          case ("", _) => false
          case _ => if text.head == pattern.head then containsPatternInner(text.tail, pattern.tail, textTail) else containsPatternInner(textTail.tail, patternText, textTail.tail)
      containsPatternInner(investigatedText, patternText, investigatedText)
    }
  }


  def reverseList[A](list: List[A]): List[A] = {
    @tailrec
    def reverseListInner(newList: List[A], oldList: List[A]): List[A] =
      oldList match
        case Nil => newList
        case head :: tail => reverseListInner(head :: newList, tail)
    reverseListInner(Nil, list)
  }

//KNP

def KNPAlgorithm(text: String, pattern: String): Int = {
  val KNP = createKNPTable(pattern)

  def indicatePattern(prefixLength: Int, position: Int): Int =
    if prefixLength + position < text.length then
      if pattern(position) == text(prefixLength + position) then
        if position == pattern.length - 1 then prefixLength else indicatePattern(prefixLength, position + 1)
      else
        if KNP(position) > -1 then indicatePattern(prefixLength + position - KNP(position), KNP(position)) else indicatePattern(prefixLength + 1, 0)
    else -1
  indicatePattern(0, 0)
}

def createKNPTable(pattern: String): Array[Int] = {
  val KNPTable = fillWithZero(pattern.length)
  KNPTable(0) = -1

  def findKNPValues(position: Int, length: Int): Array[Int] =
    if position >= pattern.length then KNPTable else
      if pattern(position - 1) == pattern(length) then
        KNPTable(position) = length + 1
        findKNPValues(position + 1, length + 1)
      else if length > 0 then findKNPValues(position, KNPTable(length))
      else
        KNPTable(position) = 0
        findKNPValues(position + 1, length)
  findKNPValues(2, 0)
}

def fillWithZero(length: Int): Array[Int] = {
  val array = Array.ofDim[Int](length)
  def fill(remainingNumberOfZerosToAdd: Int, arrayOfZero: Array[Int]): Array[Int] =
    remainingNumberOfZerosToAdd match
      case 0 => arrayOfZero
      case _ =>  arrayOfZero(remainingNumberOfZerosToAdd) = 0
                 fill(remainingNumberOfZerosToAdd - 1, arrayOfZero)
  fill(length - 1, array)
}


  //task 2

  def add3Lists[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {
      (list1, list2) match
        case (head1 :: tail1, list2) => head1 :: add3Lists(tail1, list2, list3)
        case (Nil, head2 :: tail2) => head2 :: add3Lists(Nil, tail2, list3)
        case (Nil, Nil) => list3
  }

	
  def add3ListsTail[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {
      @tailrec
      def add[T](newList: List[T], list1: List[T], list2: List[T], list3: List[T]): List[T] =
        (list1, list2, list3) match
          case (head1 :: tail1, list2, list3) => add(head1 :: newList, tail1, list2, list3)
          case (Nil, head2 :: tail2, list3) => add(head2 :: newList, Nil, tail2, list3)
          case (Nil, Nil, head3 :: tail3) => add(head3 :: newList, Nil, Nil, tail3)
          case _ => newList
      reverseList(add(List(), list1, list2, list3))
  }

   def main(args: Array[String]): Unit = {

     //task 1a tests
     println(findPattern(List("ABCD", "ADCD"), "") == List())
     println(findPattern(List("ABCD", ""), "BC") == List("ABCD"))
     println(findPattern(List("ABCD", "ADCD"), "BC") == List("ABCD"))
     println(findPattern(List("ABCD", "ADBC", "A", "BC"), "BC") == List("ABCD", "ADBC", "BC"))
     println(findPattern(List("ABCD", "ADBC"), "LD") == List())
     println(findPattern(List("ABCD", "ADBC"), "LD") == List())
     println(findPattern(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0168") == List("index0168202", "index0168211", "index0168210"))
     println()

     println(findPatternTail(List("ABCD", "ADCD"), "BC") == List("ABCD"))
     println(findPatternTail(List("ABCD", "ADBC", "A", "BC"), "BC") == List("ABCD", "ADBC", "BC"))
     println(findPatternTail(List("ABCD", "ADBC"), "LD") == List())
     println(findPatternTail(List("ABCD", "ADBC"), "LD") == List())
     println(findPatternTail(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0168") == List("index0168202", "index0168211", "index0168210"))
     println()

     //task 1b tests
     println(findPatterns(List("ABCD", "ADCD"), List("BC")) == List("ABCD"))
     println(findPatterns(List("ABCD", "ADBC", "A", "BC", "ADA"), List("BC", "AD")) == List("ABCD", "ADBC", "BC", "ADA"))
     println(findPatterns(List("ABCD", "ADBC"), List("BC", "AD")) == List("ABCD", "ADBC"))
     println(findPatterns(List("ABCD", "ADBC"), List("LD", "GB")) == List())
     println()

     println(findPatternsTail(List("ABCD", "ADCD"), List("BC")) == List("ABCD"))
     println(findPatternsTail(List("ABCD", "ADBC", "A", "BC", "ADA"), List("BC", "AD")) == List("ABCD", "ADBC", "BC", "ADA"))
     println(findPatternsTail(List("ABCD", "ADBC"), List("BC", "AD")) == List("ABCD", "ADBC"))
     println(findPatternsTail(List("ABCD", "ADBC"), List("LD", "GB")) == List())
     println()

     //task 2a tests
     println(add3Lists(List(), List(), List()) == List())
     println(add3Lists(List(1, 2), List(), List(3)) == List(1, 2,3))
     println(add3Lists(List(1), List(2, 3), List(4)) == List(1, 2, 3, 4))
     println(add3Lists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
     println()

     //task 2b tests
     println(add3ListsTail(List(), List(), List()) == List())
     println(add3ListsTail(List(1, 2), List(), List(3)) == List(1, 2, 3))
     println(add3ListsTail(List(1), List(2, 3), List(4)) == List(1, 2, 3, 4))
     println(add3ListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
   }
}


