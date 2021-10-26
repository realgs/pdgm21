def splitBySign(list: List[Int]): List[List[Int]] = {
  def split(list: List[Int], negative: List[Int], positive: List[Int]): List[List[Int]] = {
    if list == List() then List(negative, positive)
    else if list.head < 0 then split(list.tail, negative ::: List(list.head), positive)
    else if list.head > 0 && list.head % 2 != 0 then split(list.tail, negative, positive ::: List(list.head))
    else split(list.tail, negative, positive)
  }

  split(list, List(), List())
}

splitBySign(List(-3,-6,7,-9,13)) == List(List(-3,-6,-9), List(7,13))
splitBySign(List(-4,15,0,12,-12,13)) == List(List(-4,-12),List(15,13))
splitBySign(List()) == List(List(),List())


def lengthOfList[A](list: List[A]): Int = {
  if list!= List() then 1+lengthOfList(list.tail)
  else 0
}

lengthOfList(List(-4,15,0,12,-12,13)) == 6
lengthOfList(List()) == 0
lengthOfList(List("a","b","c")) == 3

def joinLists[A](xs: List[A], ys: List[A]):List[A]={
  (xs,ys) match {
    case (List(),_) => ys
    case (_,List()) => xs
    case (_,_) => xs.head :: ys.head:: joinLists(xs.tail, ys.tail)
  }
}

joinLists(List(5,4,3,2),List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6)
joinLists(List(5,4,3,2), List()) == List(5,4,3,2)
joinLists(List(), List()) == List()
