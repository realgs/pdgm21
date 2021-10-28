import scala.annotation.tailrec

//Zadanie 1
//własne reverse
def reverse[A](list: List[A]):List[A]=
  @tailrec
  def reverseIn[A](list: List[A],revList: List[A]):List[A]=
    list match
      case Nil=>revList
      case head::tail=>reverseIn(list.tail,head::revList)
  reverseIn(list,Nil)

reverse(List(-3,-6,7,-9,13))==List(13,-9,7,-6,-3)
reverse(List())==List()
reverse(List("kota","ma","Ala"))==List("Ala","ma","kota")
reverse(List(1))==List(1)

def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  @tailrec
  def splitBySignIn(list: List[Int], negativeNum: List[Int], positiveNum: List[Int]): (List[Int], List[Int]) =
    list match
      case Nil => (negativeNum, positiveNum)
      case head :: tail =>
                          if head < 0 then splitBySignIn(tail, head :: negativeNum, positiveNum)
                          else if ((head > 0) && (head % 2 == 1)) then splitBySignIn(tail,negativeNum,head::positiveNum)
                          else splitBySignIn(tail,negativeNum,positiveNum)
  splitBySignIn(reverse(list),Nil,Nil)

splitBySign(List(-3,-6,7,-9,13))==(List(-3,-6,-9),List(7,13))
splitBySign(List(0,2,1,-4,-1))==(List(-4,-1),List(1))
splitBySign(List())==(List(),List())
splitBySign(List(-1,-2,-3,-1))==(List(-1,-2,-3,-1),List())
splitBySign(List(1,3,5))==(List(),List(1,3,5))

//Zadanie 2
def lengthOfList[A](list: List[A]): Int = {
  if list == Nil then 0
  else 1 + lengthOfList(list.tail)
}
lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List()) == 0
lengthOfList(List("Ala", "ma", "kota")) == 3

//Zadanie 3
def joinLists[A](list1: List[A], list2: List[A]): List[A] =
  (list1, list2) match {
    case (Nil, Nil) => Nil
    case (head :: tail, Nil) => head :: joinLists(tail, Nil)
    case (Nil, head :: tail) => head :: joinLists(Nil, tail)
    case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: joinLists(tail1, tail2)
  }
joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(), List()) == Nil
joinLists(List(1, 2, 3), List()) == List(1, 2, 3)
joinLists(List(), List(1, 2, 3)) == List(1, 2, 3)
joinLists(List("Ala", "kota"), List("ma")) == List("Ala", "ma", "kota")

