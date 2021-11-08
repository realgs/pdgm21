import scala.annotation.tailrec

object list4 {
  def reversing[A](list:List[A]): List[A]={
    if list!=Nil then reversing(list.tail):+list.head
    else Nil
  }
  //tailrec
  def find(list: List[String], toFind: String): List[String] = {
    @tailrec
    def finding(result: List[String], list1: List[String], toFind: String): List[String] = {
      if list1 == Nil then result
      else if list1.head.length >= toFind.length then if pattern(list1.head, toFind, 0) == toFind.length then finding(list1.head :: result, list1.tail, toFind) else finding(result, list1.tail, toFind)
      else finding(result, list1.tail, toFind)
    }

    def pattern(elm: String, patt: String, result: Int): Int = {
      if patt.length == 0 then result
      else if elm.length == 0 then result
      else if elm.head == patt.head then pattern(elm.tail, patt.tail, result + 1)
      else if elm.head == toFind.head then pattern(elm.tail, toFind.tail, 1)
      else pattern(elm.tail, toFind, 0)
    }

    reversing(finding(List(), list, toFind))
  }
  //zlozonosc obliczeniowa O(n^2)
  //ze wzgledu na mnozenie dlugosci listy i dlugosci slowa
  //zlozonosc pamieciowa O(1)
  //ze wzgledu na rekursje ogonowa

  //without tailrec
  def find2(list: List[String], toFind: String): List[String] = {
    def pattern(elm: String, patt: String): Boolean = {
      if patt.length == 0 then true
      else if elm.length == 0 then false
      else if elm.head == patt.head then pattern(elm.tail, patt.tail)
      else if elm.head == toFind.head then pattern(elm.tail, toFind.tail)
      else pattern(elm.tail, toFind)
    }
    if list != Nil then if pattern(list.head, toFind) then list.head :: find2(list.tail, toFind) else find2(list.tail, toFind)
    else Nil
  }
  //zlozonosc obliczeniowa O(n^2)
  //taki sam przypadek co powyzej
  //zlozonosc pamieciowa O(n)
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
  //zlozonosc pamieciowa O(1)
  //ze wzgledu na rekursje ogonowa

  //without tailrec
  def joinList2[A](list1: List[A], list2: List[A], list3: List[A]): List[A] ={
    if list2!=Nil then joinList2(list1:+list2.head, list2.tail, list3)
    else if list3!=Nil then joinList2(list1:+list3.head, list2, list3.tail)
    else list1
  }
  //zlozonosc obliczeniowa O(n)
  //tak jak powyzej
  //zlozonosc pamieciowa O(n)
  //ze wzgledu na laczenie list2 i list3

  def main(args: Array[String]): Unit = {
    println("Find in list:")
    println("tail rec")
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println(find(List("12", "12578", "11123", "9012334"), "123"))
    println(find(List("k", "l", "m"), "m"))
    println("rec")
    println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println(find2(List("12", "12578", "11123", "9012334"), "123"))
    println(find2(List("k", "l", "m"), "m"))
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
