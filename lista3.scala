import scala.annotation.tailrec
//Sebastian Bednarski

//zadanie 1
def splitBySign(list: List[Int]): (List[List[Int]]) =
  @tailrec
  def splitBySignIter(list: List[Int], negative: List[Int], positiveOdd : List[Int]) : List[List[Int]] =
    if list == List() then List(negative, positiveOdd)
    else if ((list.head > 0) && (list.head % 2 != 0)) then splitBySignIter(list.tail, negative, positiveOdd ::: List(list.head))
    else if list.head < 0 then splitBySignIter(list.tail, negative ::: List(list.head), positiveOdd)
    else splitBySignIter(list.tail, negative, positiveOdd)
  splitBySignIter(list, List(), List())

splitBySign(List(-3,-6,7,-9,13)) == List(List(-3, -6, -9), List(7, 13))
splitBySign(List(-1,8,-7,21,-13)) == List(List(-1, -7, -13), List(21))
splitBySign(List()) == List(List(), List())
//zadanie 2
def listLength[A](list: List[A]): Int =
  @tailrec
  def listLengthIter[A](list: List[A], length: Int): Int =
    if(list == Nil) then length
    else listLengthIter(list.tail, length + 1)
  listLengthIter(list, 0)

listLength(List(5, 4, 3, 2)) == 4
listLength(List((-1), 2, 3, 4, 5)) == 5
listLength(List("a", "l", "a")) == 3
listLength(List()) == 0

//zadanie 3
def listMerge[A] (list1:List[A], list2:List[A]): List[A] =
  @tailrec
  def listMergeIter(accum: List[A], list1:List[A], list2:List[A]): List[A] =
    if list1 == List() then accum ::: list2
    else if list2 == List() then accum ::: list1
    else listMergeIter(accum ::: List(list1.head) ::: List(list2.head), list1.tail, list2.tail)
  listMergeIter(List(), list1, list2)

listMerge(List(5,4,3,2),List(1,2,3,4,5,6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
listMerge(List(),List(1,2,3,4,5,6)) == List(1, 2, 3, 4, 5, 6)
listMerge(List(5,4,3,2),List()) == List(5, 4, 3, 2)
listMerge(List("A","l","a"),List("k","o","t","a")) == List("A", "k", "l", "o", "a", "t", "a")
