import scala.annotation.tailrec

object list4 {
  def reversing[A](list:List[A]): List[A]={
    if list!=Nil then reversing(list.tail):+list.head
    else Nil
  }
  def find(list: List[String], patternList: List[String]): List[String]={
    @tailrec
    def checkElement(elem: String, pattern: String, orginalPattern: String): Boolean={
      if elem.length < pattern.length then false
      else if pattern=="" then true
      else if elem.head==pattern.head then checkElement(elem.tail, pattern.tail, orginalPattern)
      else if elem.head==orginalPattern.head then checkElement(elem.tail, orginalPattern.tail, orginalPattern)
      else checkElement(elem.tail, orginalPattern, orginalPattern)
    }
    def checkElementwithPatterns(elem: String, patterns: List[String]): Boolean={
      if patterns==Nil then false
      else if checkElement(elem, patterns.head, patterns.head)==true then true
      else checkElementwithPatterns(elem, patterns.tail)
    }
    def checkAllElements(toCheckList: List[String], patternList2: List[String], resultList: List[String]): List[String]={
      toCheckList match {
        case Nil=> resultList
        case h::t =>if checkElementwithPatterns(h, patternList2)==true then checkAllElements(t, patternList2, h::resultList) else checkAllElements(t, patternList2, resultList)
      }
    }
    checkAllElements(list, patternList, List())
  }
  //zlozonosc obliczeniowa O(n^3)
  //ze wzgledu na mnozenie dlugosci listy do sprawdzenia, dlugosc listy wzorcow i dlugosci slowa
  //zlozonosc pamieciowa O(n)
  //ze wzgledu na rekursje ogonowa n- dlugosc listy wyjsciowej

  //without tailrec
  def findNoTail(toCheckList: List[String], patternList: List[String]): List[String]={
    def checkElement(elem: String, pattern: String, orginalPattern: String): Boolean={
      if elem.length < pattern.length then false
      else if pattern=="" then true
      else if elem.head==pattern.head then checkElement(elem.tail, pattern.tail, orginalPattern)
      else if elem.head==orginalPattern.head then checkElement(elem.tail, orginalPattern.tail, orginalPattern)
      else checkElement(elem.tail, orginalPattern, orginalPattern)
    }
    def checkElementwithPatterns(elem: String, patterns: List[String]): Boolean={
      if patterns==Nil then false
      else if checkElement(elem, patterns.head, patterns.head)==true then true
      else checkElementwithPatterns(elem, patterns.tail)
    }
    toCheckList match{
      case Nil => Nil
      case h::t =>if checkElementwithPatterns(h, patternList)==true then h::findNoTail(toCheckList.tail, patternList) else findNoTail(toCheckList.tail, patternList)
    }

  }
  //zlozonosc obliczeniowa O(n^3)
  //taki sam przypadek co powyzej
  //zlozonosc pamieciowa O(n^2)
  //ze wzgledu na ::

  //tailrec
  def joinList[A](list1: List[A], list2: List[A], list3: List[A]): List[A] ={
    @tailrec
    def joining[A](result:List[A], list2a: List[A], list3a: List[A]): List[A] ={
      if list2a!=Nil then joining(result:+list2a.head, list2a.tail, list3a)
      else if list3a!=Nil then joining(result:+list3a.head, list2a, list3a.tail)
      else result
    }
    joining(list1, list2, list3)
  }
  //zlozonosc obliczeniowa O(n)
  //bedzie to suma dlugosci list2 i list3
  //zlozonosc pamieciowa O(n)
  //ze wzgledu na rekursje ogonowa

  //without tailrec
  def joinList2[A](list1: List[A], list2: List[A], list3: List[A]): List[A] ={
    if list2!=Nil then joinList2(list1:+list2.head, list2.tail, list3)
    else if list3!=Nil then joinList2(list1:+list3.head, list2, list3.tail)
    else list1
  }
  //zlozonosc obliczeniowa O(n)
  //tak jak powyzej
  //zlozonosc pamieciowa O(n^2)
  //ze wzgledu na laczenie list2 i list3

  def main(args: Array[String]): Unit = {
    println("Find in list:")
    println("tail rec")
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")))
    println(find(List("12", "12578", "11123", "9012334"), List("125", "34")))
    println(find(List("anakonda", "kot", "ania", "oto", "mania"), List("ania", "ana")))
    println("rec")
    println(findNoTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")))
    println(findNoTail(List("12", "12578", "11123", "9012334"), List("125", "34")))
    println(findNoTail(List("anakonda", "kot", "ania", "oto", "mania"), List("ania", "ana")))
    println()
    println("Join list:")
    println("tail rec")
    println(joinList(List(5, 4, 3, 2), List(1, 0), List(9, 8)))
    println(joinList(List("a", "b", "c", "d"), List("e", "f", "g"), List("h", "i", "j", "k")))
    println(joinList(List(Nil),List(Nil), List(Nil)))
    println("rec")
    println(joinList2(List(5, 4, 3, 2), List(1, 0), List(9, 8)))
    println(joinList2(List("a", "b", "c", "d"), List("e", "f", "g"), List("h", "i", "j", "k")))
    println(joinList(List(Nil),List(Nil), List(Nil)))
  }
}
