object lista3 extends App{
  // zadanie 1
  // zlozonosc czasowa: n
  def splitBySign (myList:List[Int]):(List[Int], List[Int]) = {
    def splitBySignHelper (myList:List[Int],negative:List[Int],positiveAndOdd: List[Int]):
    (List[Int], List[Int]) = {
      if myList == List() then (negative.reverse, positiveAndOdd.reverse)
      else if myList.head < 0 then splitBySignHelper(myList.tail, myList.head::negative, positiveAndOdd)
      else if (myList.head>0 && myList.head % 2 == 1) then
        splitBySignHelper(myList.tail, negative, myList.head::positiveAndOdd)
      else splitBySignHelper(myList.tail, negative, positiveAndOdd)
    }
    splitBySignHelper(myList, List(), List())
  }

  println("zadanie 1")
  println(splitBySign(List(-3,-6,7,-9,13)) == (List(-3,-6,-9),List(7,13)))
  println(splitBySign(List()) == (List(),List()))
  println(splitBySign(List(0,6)) == (List(),List()))

  //zadanie 2
  // zlozonosc czasowa: n
  def lengthOfList[A] (myList:List[A]):Int = {
    ((myList).foldLeft(0))((length, x)=>length+1)
  }

  println("zadanie 2")
  println(lengthOfList(List(5,4,3,2)) == 4)
  println(lengthOfList(List()) == 0)
  println(lengthOfList(List(5)) == 1)

  // zadanie 3
  // zlozonosc czasowa: n
  def joinLists[A] (myList1:List[A], myList2:List[A]):List[A] = {
    if myList1 == List() then myList2
    else if myList2 == List() then myList1
    else myList1.head::joinLists(myList2, myList1.tail)
  }

  println("zadanie 3")
  println(joinLists(List(5,4,3,2), List(1,2,3,4,5,6))==List(5,1,4,2,3,3,2,4,5,6))
  println(joinLists(List(), List())==List())
  println(joinLists(List(9), List())==List(9))
  println(joinLists(List(), List(9,8))==List(9,8))
  println(joinLists(List(1,3), List(2))==List(1,2,3))
}
