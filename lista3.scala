
object l3 {
  def main(args: Array[String]):Unit  = {
    println("Zadanie 1")
    println(splitBySign(List(-3,-6,7,-9,13)))
    println(splitBySign(List(-1,3,8,-9,-2)))
    println(splitBySign(List()))

    println("Zadanie 2")
    println(lengthOfList(List(5,4,3,2)))
    println(lengthOfList(List(1,2,3,4,7,8,9)))
    println(lengthOfList(List()))
    println(lengthOfList(List('A')))

    println("Zadanie 3")
    println(joinLists(List(5,4,3,2), List(1,2,3,4,5,6)))
    println(joinLists(List(1,2,3), List(3,4,5)))
    println(joinLists(List('A','B','C'), List('E','F','G')))
    println(joinLists(List(), List(20,30,40)))
    println(joinLists(List(), List()))
    println(joinLists(List("Ala", "ma", "kota"), List()))

/*
    println("druga wersja")
    println(joinLists1(List(5,4,3,2), List(1,2,3,4,5,6)))
    println(joinLists1(List(1,2,3), List(3,4,5)))
    println(joinLists1(List('A','B','C'), List('E','F','G')))
*/

  }

  //zadanie 1
  def splitBySign(list:List[Int]): (List[Int], List[Int]) =
    list match
      case Nil => (Nil, Nil)
      case h::t =>
        val (negative, posOdd) = splitBySign(t)
        if h < 0 then (h::negative, posOdd)
        else if (h>0 && h%2!=0) then (negative, h::posOdd)
        else (negative, posOdd)



  //zadanie 2
  def lengthOfList[A] (list:List[A]): Int =
    if list == Nil then 0
    else  1 + lengthOfList(list.tail)


  //zadanie 3

  def joinLists[A](list1:List[A], list2:List[A]): List[A] =
    (list1, list2) match
      case (Nil, Nil) => Nil
      case (_, Nil) => list1
      case (Nil, _) => list2
      case (h1::t1, h2::t2) => h1::h2::joinLists(t1, t2)


  //druga wersja
  def joinLists1[A] (list:List[A], list2:List[A]): List[A] =
    (list) match
      case (Nil) => list2
      case (h::list) => h:: joinLists(list2, list)
}

