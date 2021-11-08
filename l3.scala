object l3 {
  //1
  //O(n^2)
  //Memory O(1)
  def splitBySign[A](xs: List[Int])={
    def split(xs: List[Int], negativeList:List[Int], positiveList:List[Int]):(List[Int], List[Int])={
      if xs ==Nil then (negativeList, positiveList)
      else if xs.head<0 then split(xs.tail, negativeList:::List(xs.head), positiveList)
      else if xs.head>0 && xs.head %2==1 then split(xs.tail, negativeList, positiveList:::List(xs.head))
      else split(xs.tail, negativeList,  positiveList)
    }
    split(xs, List(), List())
  }

  //2
  //O(n)
  //Memory O(1)
  def lengthOfList[A](xs: List[A]): Int={
    if xs!=Nil then 1+lengthOfList(xs.tail)
    else 0
  }

  //3
  //O(n)
  //Memory O(n)
  def joinList[A](xs: List[A], ys: List[A]): List[A]={
    if xs!=Nil && ys!=Nil then xs.head::ys.head::joinList(xs.tail, ys.tail)
    else if xs!=Nil && ys==Nil then xs
    else if xs==Nil && ys!=Nil then ys
    else Nil
  }

  def l3(args: Array[String]): Unit = {
    println(splitBySign(List(1,2,-3, 10,-7,9,10,-11, 13)))
    println(splitBySign(List()))
    println(splitBySign(List(1,1,-1,-1,1,10,-11,11,12,14)))
    println(" ")
    println(lengthOfList(List(5,4,3,2)))
    println(lengthOfList(List()))
    println(lengthOfList(List(1,2,3,4,5,6,7,8,9)))
    println(" ")
    println(joinList(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)))
    println(joinList(List(1,2,3,4,5,6,7), List(0,11)))
    println(joinList(List(), List()))
    println(" ")
  }
}
