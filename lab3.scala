import scala.annotation.tailrec

object Lista3 {

  val splitBySign: List[Int] => (List[Int], List[Int]) = entryList =>
    @tailrec
    def splitBySignIter (entryList: List[Int], ac: (List[Int], List[Int])): (List[Int], List[Int])=
      if(entryList == Nil) then ac
      else if(entryList.head < 0) then splitBySignIter(entryList.tail, (ac._1 ::: List(entryList.head), ac._2))
      else if(entryList.head > 0 && entryList.head % 2 != 0) then splitBySignIter(entryList.tail, (ac._1, ac._2 ::: List(entryList.head)))
      else splitBySignIter(entryList.tail, ac)
    splitBySignIter(entryList, (List(), List()))

  def lengthOfList[A](entryList: List[A]): Int=
    @tailrec
    def lengthOfListIter[A](entryList: List[A], ac: Int): Int=
      if(entryList == Nil) then ac
      else lengthOfListIter(entryList.tail, ac+1)
    lengthOfListIter(entryList, 0)

  def joinLists[A](firstList: List[A], secondList: List[A]): List[A]=
    @tailrec
    def joinListsIter[A](firstList: List[A], secondList: List[A], ac: List[A]): List[A]=
      if(firstList == Nil && secondList == Nil) then ac
      else if (firstList == Nil) then ac ::: secondList
      else if (secondList == Nil) then ac ::: firstList
      else joinListsIter(firstList.tail, secondList.tail, ac ::: List(firstList.head) ::: List(secondList.head))
    joinListsIter(firstList, secondList, List())

  def main(args: Array[String]): Unit = {

    println(splitBySign(List(-3,-6,7,-9,13)) == (List(-3, -6, -9),List(7, 13)))
    println(splitBySign(List()) == (List(),List()))
    println(splitBySign(List(-3,-9,-18,0,2,4,6,8,3,-5,5,7,9,11)) == (List(-3, -9, -18, -5),List(3, 5, 7, 9, 11)))

    println(lengthOfList(List(-3,-6,7,-9,13)) == 5)
    println(lengthOfList(List()) == 0)
    println(lengthOfList((List("Ala", "ma", "kota"))) == 3)

    println(joinLists(List(-3,-6,7,-9,13), List(55,80,15,20,30,15,25,45)) == List(-3, 55, -6, 80, 7, 15, -9, 20, 13, 30, 15, 25, 45))
    println(joinLists(List(),List("Ala", "ma", "kota")) == List("Ala", "ma", "kota"))
    println(joinLists(List("Ala", "ma", "kota"), List()) == List("Ala", "ma", "kota"))
    println(joinLists(List(), List()) == List())
  }
}

