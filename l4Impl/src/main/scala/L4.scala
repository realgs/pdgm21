import scala.annotation.tailrec

object L4 {

  // 1
  // time complexity: O(n)
  // space complexity: O(1)
  def myLength[A](list : List[A]) : Int = {
    @tailrec
    def myLenghtIn(list : List[A], i: Int) : Int = {
      if list == List() then i
      else myLenghtIn(list.tail, i+1)
    }
    myLenghtIn(list, 0)
  }

  // time complexity: O(n)
  // space complexity: O(1)
  @tailrec
  def myContains(word : String, value : String) : Boolean = {
    if value == "" then true
    else if word == "" then false
    else if word.head == value.head then myContains(word.tail, value.tail)
    else myContains(word.tail, value)
  }

  // time complexity: O(n^2) - myContains is O(n)
  // space complexity: O(1)
  @tailrec
  def containsValues(word : String, searchValues : List[String]) : Boolean = {
    if searchValues == List() then false
    if myContains(word, searchValues.head) then true
    else containsValues(word, searchValues.tail)
  }

  // time complexity: O(n^2) - myContains is O(n)
  // space complexity: O(n)
  def findValue(list : List[String], searchValue : String) : List[String] = {
    @tailrec
    def findValueIn(list : List[String], searchValue : String, result : List[String]) : List[String] = {
      if list == List() then result
      else if myContains(list.head, searchValue) then findValueIn(list.tail, searchValue, list.head :: result)
      else findValueIn(list.tail, searchValue, result)
    }
    findValueIn(list, searchValue, List())
  }

  // time complexity: O(n^2)
  // space complexity: n + n(n + 1)/2 + 1 = 0,5n^2 + 1,5n + 1 => polynominal
  def findValue2(list : List[String], searchValue : String) : List[String] = {
    if list == List() then List()
    if myContains(list.head, searchValue) then list.head :: findValue(list.tail, searchValue)
    else "" :: findValue(list.tail, searchValue)
  }

  // time complexity: O(n^3) - containsValues is O(n^2)
  // space complexity: O(n)
  def findMultipleValues(list : List[String], searchValues : List[String]) : List[String] = {
    @tailrec
    def findMultipleValuesIn(list : List[String], searchValues : List[String], result : List[String]) : List[String] = {
      if list == List() then result
      else if containsValues(list.head, searchValues) then findMultipleValuesIn(list.tail, searchValues, list.head :: result)
      else findMultipleValuesIn(list.tail, searchValues, result)
    }
    findMultipleValuesIn(list, searchValues, List())
  }

  // time complexity: O(n^3)
  // space complexity: n + n(n + 1)/2 + n * m = 0,5n^2 + n(0,5 + m) => polynominal,
  //                   where n - length of list, m - length of searchValues
  def findMultipleValues2(list : List[String], searchValues : List[String]) : List[String] = {
    if list == List() then List()
    else if containsValues(list.head, searchValues) then list.head :: findMultipleValues(list.tail, searchValues)
    else "" :: findMultipleValues(list.tail, searchValues)
  }


  // 2

  // time complexity: O(n)
  // space complexity: O(n)
  def myReverse[A](list : List[A]) : List[A] = {
    @tailrec
    def myReverseIn(list : List[A], resultList : List[A]) : List[A] = {
      if list == List() then resultList
      else myReverseIn(list.tail, list.head :: resultList)
    }
    myReverseIn(list, List())
  }

  // time complexity: O(n + m)
  // space complexity: n + n(n+1)/2 + n*m + n*l + m + m(m+1)/2 + m*l = 0,5(n^2 + m^2) + n(m + l + 1,5) + m(l + 1,5) =>
  //                   polynominal, where
  //                   n - length of list1
  //                   m - length of list2
  //                   l - length of list3
  def joinLists[A](list1 : List[A], list2 : List[A], list3 : List[A]) : List[A] = {
    (list1, list2, list3) match {
      case (h::t, _, _) => h :: joinLists(t, list2, list3)
      case (List(), h::t, _) => h :: joinLists(List(), t, list3)
      case (List(), List(), _) => list3
    }
  }

  // time complexity: O(n + m)
  // space complexity: O(n)
  def joinLists2[A](list1 : List[A], list2 : List[A], list3 : List[A]) : List[A] = {
    @tailrec
    def joinList2In[A](list1 : List[A], list2 : List[A], list3 : List[A], acc : List[A]) : List[A] = {
      (list1, list2, list3) match {
        case (h::t, _, _) => joinList2In(t, list2, list3, h :: acc)
        case (List(), h::t, _) => joinList2In(List(), t, list3, h :: acc)
        case (List(), List(), h::t) => joinList2In(List(), List(), t, h :: acc)
        case (List(), List(), List()) => myReverse(acc)
      }
    }
    joinList2In(list1, list2, list3, List())
  }


  def main(args: Array[String]) : Unit = {
    println(joinLists(List(1,2,3), List(4,5,6), List(7,8,9)))
    println(joinLists2(List(1,2,3), List(4,5,6), List(7,8,9)))
    println(findValue(List("1234567", "123", "12", "354123355", "35352123", "4342412543"), "123"))
    println(findValue2(List("1234567", "123", "12", "354123355", "35352123", "4342412543"), "123"))
    println(findMultipleValues(List("1234567", "123", "12", "354123355", "35352123", "4342412543"), List("123", "1")))
    println(findMultipleValues2(List("1234567", "123", "12", "354123355", "35352123", "4342412543"), List("123", "1")))
  }

}
