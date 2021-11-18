import scala.annotation.tailrec

object l4 {

  def reverseList[A](list: List[A]):List[A] = {
    @tailrec
    def reverse[A](list: List[A], reversedList: List[A]):List[A] = {
      list match
        case h::t => reverse(t,h::reversedList)
        case Nil => reversedList
    }
    reverse(list, List())
  }

  /*złożoność obliczeniowa O(n*m*k) n,m - długości list, k - suma długośći wyrazów
    złożoność pamięciowa O(n^2)
     l + n(n+1)/2 = (n^2 + n)/2 + l, l - długość listy wynikowej*/
  def find(list: List[String], elements: List[String]): List[String] = {
    list match {
      case h::t => if containsElements(elements,h) then h::find(t, elements) else find(t, elements)
      case Nil => List()
    }
  }

  /*złożoność obliczeniowa O(n*m*k) n,m - długości list, k - suma długośći wyrazów
    złożoność pamięciowa O(n) n - długość listy wynikowej*/
  def find2(list:List[String], elements: List[String]): List[String] = {
    @tailrec
    def findTail(list: List[String], elements: List[String], results: List[String]): List[String] = {
      list match
        case h::t => if containsElements(elements,h) then findTail(t,elements,h::results) else findTail(t,elements,results)
        case Nil => results
    }
    reverseList(findTail(list, elements,List()))
  }
  def containsElement(element: String, word: String): Boolean = {
     if element.isBlank then true
     else if word.isBlank then false
     else if element.head == word.head then containsElement(element.tail,word.tail)
     else containsElement(element,word.tail)
  }
  def containsElements(elements: List[String], word: String): Boolean = {
    if elements == Nil then false
    else containsElement(elements.head, word) || containsElements(elements.tail,word)
  }
  /*zlożoność obliczeniowa O(n+m); n,m - długości list 1,2
    złożoność pamięciowa O(n^2)
      (n+(n+1)n/2) + (n+(n+1)n/2) + m = n^2 + 3n + m; n-długość list 1,2; m - długość listy 3*/
  def joinList[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2) match
      case (h::t, list2) => h :: joinList(t,list2,list3)
      case(Nil, h::t) => h::joinList(Nil, t, list3)
      case(Nil, Nil) => list3
  }
  /*zlożoność obliczeniowa O(2(n+m+k)); n,m,k - długości list 1,2,3; reverse
    złożoność pamięciowa O(n); n - suma długości list 1,2,3 */
  def joinList2[A](list1: List[A], list2: List[A], list3: List[A]):List[A] = {
    @tailrec
    def joinListTail[A](list1: List[A], list2: List[A], list3: List[A], list: List[A]): List[A] = {
      (list1, list2, list3) match
        case (h::t, list2, list3) => joinListTail(t, list2, list3, h::list)
        case (Nil, h::t, list3) => joinListTail(Nil, t, list3, h::list)
        case (Nil, Nil, h::t) => joinListTail(Nil, Nil, t, h::list)
        case(Nil, Nil, Nil) => list

    }
    reverseList(joinListTail(list1, list2, list3, List()))
  }


  def main(args: Array[String]): Unit = {
    println(joinList(List(5,4,3,2),List(1,0),List(9)))
    println(joinList(List('a','b'), List('c','d'), List('e')))
    println(joinList(List('a','b'), List('c','d'), List()))
    println()
    println(joinList2(List(5,4,3,2),List(1,0),List(9)))
    println(joinList2(List('a','b'), List('c','d'), List('e')))
    println(joinList(List('a','b'), List(), List('e')))
    println()
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168","index0169224")))
    println(find2(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("iindex016","index0169224")))
    println(find2(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("iiindex016","inndex0169224")))

  }

}
