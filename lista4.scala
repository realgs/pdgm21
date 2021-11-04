object Lab4 {
  
  def contains(word: String, element: String, curr1: Int, curr2: Int) : Boolean = {
    if curr2==element.length then true
    else if curr1>=word.length then false
    else if word.charAt(curr1) == element.charAt(curr2) then contains(word, element, curr1+1, curr2+1)
    else contains(word, element, curr1+1, 0)
  }
  
  def iterateElements(elements: List[String], head: String) : String = {
    elements match{
            case element::otherElements => 
              if element=="" then iterateElements(otherElements, head)
              else if contains(head, element, 0, 0) then head
              else iterateElements(otherElements, head)
            case Nil => ""
          }
  }
  
  def findN(input: List[String], elements: List[String]) : List[String] = {
      input match {
        case head::tail => val result = iterateElements(elements, head)
        if result != "" then result::findN(tail, elements)
        else findN(tail, elements)
        case Nil => List()
      }
  }
  

  
  def reverse[A](list: List[A], current: List[A]) : List[A] = {
    list match{
      case head::tail => reverse(tail, head::current)
      case Nil => current
    }
  }
  
  def join3Lists[A](list1: List[A], list2: List[A], list3: List[A]) : List[A] = {
    def helper(list1: List[A], list2: List[A], list3: List[A], current: List[A]) : List[A] = {
        list1 match {
          case head::tail => helper(tail, list2, list3, head::current)
          case Nil => list2 match {
            case head2::tail2 => helper(list1, tail2, list3, head2::current)
            case Nil => list3 match {
              case head3::tail3 => helper(list1, list2, tail3, head3::current)
              case Nil => current
          }
        }
      }
    }
    reverse(helper(list1, list2, list3, List()), List())
  }
  
  
  def main(args: Array[String]): Unit = {
    println(findN(List("aaaabc", "bcd", "cde", "google", ""), List("bc", "de")))
    println(join3Lists(List(1,2,3), List(4,5,6), List(7,8,9)))
    println(join3Lists(List(), List(4,5,6), List(7,8,9)))
    println(join3ListsBetter(List(1,2,3), List(4,5,6), List(7,8,9)))
  }
}