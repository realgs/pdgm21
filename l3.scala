object l3 {

  //task 1
  def splitBySign(numList: List[Int]): List[List[Int]] =
    def split (numList: List[Int], negativeNumList: List[Int], positiveOddNumList: List[Int]): List[List[Int]] =
      if numList == Nil then List(negativeNumList, positiveOddNumList)
      else if numList.head < 0 then split(numList.tail, negativeNumList ::: List(numList.head), positiveOddNumList)
      else if numList.head % 2 == 1 then split(numList.tail, negativeNumList, positiveOddNumList ::: List( numList.head))
      else split (numList.tail, negativeNumList, positiveOddNumList)

    split(numList, List(), List())

  //task 2
  def lengthOfList[A](list: List[A]): Int =
    if list == Nil then 0 else 1 + lengthOfList(list.tail)

  //task 3
  def joinLists[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match
      case (Nil, _) => list2
      case (_, Nil) => list1
      case _ => list1.head :: list2.head :: joinLists(list1.tail, list2.tail)

  //main
  def main () = {
    println("splitBySign(List(-3, -6, 7, -9, 13)): " + splitBySign(List(-3, -6, 7, -9, 13)))
    println("splitBySign(Nil): " + splitBySign(Nil))
    println("splitBySign(List(1, 2, -3, -4, 5, 6, -7, -8, 9, 10)): " + splitBySign(List(1, 2, -3, -4, 5, 6, -7, -8, 9, 10)))

    println("lengthOfList(List(5, 4, 3, 2)): " + lengthOfList(List(5, 4, 3, 2)))
    println("lengthOfList(List(\"c\", \"b\", \"a\")): " + lengthOfList(List("c", "b", "a")))
    println("lengthOfList(List()): " + lengthOfList(List()))

    println("joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)): " + joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)))
    println("joinLists(List(\"Ala\", \"kota\"), List(\"ma\")): " + joinLists(List("Ala", "kota"), List("ma")))
    println("joinLists(Nil, Nil): " + joinLists(Nil, Nil))
  }
}
