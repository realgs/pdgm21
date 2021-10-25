import scala.annotation.tailrec

def reverseList[A](list :List[A]) = {
  def reverseListHelper[A](srcList :List[A], destList :List[A]):List[A] =
    if srcList == Nil then destList
    else reverseListHelper(srcList.tail, srcList.head::destList)


  reverseListHelper(list,Nil)
}


def splitBySign(list :List[Int]) = {
  def splitHelper(srclist: List[Int], negative: List[Int], oddPositive: List[Int]): List[List[Int]] =
    srclist match
      case Nil => List(reverseList(negative),reverseList(oddPositive))
      case (head :: tail) if head < 0 => splitHelper(srclist.tail, head :: negative, oddPositive)
      case (head :: tail) if (head > 0 && head % 2 == 1) => splitHelper(srclist.tail, negative, head :: oddPositive)
      case _ => splitHelper(list.tail, negative, oddPositive)

  splitHelper(list,Nil,Nil)
}


splitBySign(List())
splitBySign(List(-3,-6,7,-9,13))
//splitBySign(List(-3,-6,7,-9,13,14))


def listLength[A](list :List[A]) = {
  def listLengthHelper[A](list :List[A], sum :Int):Int =
    if list == Nil then sum
    else listLengthHelper(list.tail, sum + 1)

  listLengthHelper(list,0)
}

listLength(List(1,2,3,4,5,6,7))
listLength(List(Nil))
listLength(List("A","B","C"))


def joinLists[A](first :List[A], second :List[A]) = {
  def joinListsHelper[A](first :List[A], second :List[A], result :List[A]):List[A] =
    (first,second) match
      case (Nil,Nil) => result
      case (Nil,_) => reverseList(second) ::: result
      case (_,Nil) => reverseList(first) ::: result
      case _ =>  joinListsHelper(first.tail,second.tail,second.head :: first.head :: result)


  reverseList(joinListsHelper(first,second,Nil))
}

joinLists(List(5,4,3,2),List(1,2,3,4,5,6))
joinLists(List(),List(1,2,3,4))
joinLists(List("A","B","C"),List())



