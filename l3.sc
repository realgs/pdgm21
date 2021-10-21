//zadanie 1
def splitBySign(xs:List[Int]): (List[Int], List[Int]) =
  def splitBySignIter(ys:List[Int], l1:List[Int], l2:List[Int]): (List[Int], List[Int]) =
    if ys == Nil then (l1,l2) else
      if ys.head < 0 then splitBySignIter(ys.tail,ys.head::l1,l2) else
        if ys.head % 2 == 1 then splitBySignIter(ys.tail, l1, ys.head::l2) else
          splitBySignIter(ys.tail,l1,l2)
  splitBySignIter(xs,Nil,Nil)

splitBySign(List(1,2,3,-1,-2,-3,4,5,6,0,-2))

//zadanie 2
def lengthOfList[A](xs:List[A]): Int =
  if xs == Nil then 0 else
    1 + lengthOfList(xs.tail)

lengthOfList(Nil)
lengthOfList(List(1,2,3))

//zadanie 3
def joinLists[A](xs:List[A], ys:List[A]): List[A] =
  (xs, ys) match
    case (Nil,Nil) => Nil
    case (Nil, _) => ys.head::joinLists(xs,ys.tail)
    case (_, Nil) => xs.head::joinLists(xs.tail,ys)
    case _ => xs.head::ys.head::joinLists(xs.tail, ys.tail)

joinLists(List(1,2,3),List("a","b","c","d"))
joinLists(List(1,2,3),Nil)
joinLists(Nil,List(1,2,3))