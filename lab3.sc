

//zadanie1

def splitBySign(xs:List[Int]):(List[Int],List[Int])=
  def splitBySignIter(xs:List[Int],plus:List[Int],minus:List[Int]):(List[Int],List[Int]) =
    if xs==Nil then (plus,minus)
    else if xs.head < 0 then splitBySignIter(xs.tail,plus,minus:::List(xs.head))
    else if xs.head % 2 == 0 then splitBySignIter(xs.tail,plus,minus)
    else splitBySignIter(xs.tail,plus:::List(xs.head),minus)
  splitBySignIter(xs,List(),List())

splitBySign(List(1,-2,3,4,-5)) == (List(1, 3),List(-2, -5))
splitBySign(List(1,2,3,4,5)) == (List(1, 3, 5),List())
splitBySign(List(-1,-2,-3,-4,-5)) == (List(),List(-1, -2, -3, -4, -5))
splitBySign(List()) == (List(),List())

//złożoność obliczeniowa O(n^2)
//złożoność pamięciowa O(n)

//zadanie2
def lengthOfList[A](xs:List[A]):Int=
  if xs==Nil then 0
  else 1+lengthOfList(xs.tail)

lengthOfList(List(1,2,3)) == 3
lengthOfList(List('a','b','c','d')) == 4
lengthOfList(List()) == 0

// złożoność obliczeniowa O(n)
// złożoność pamięciowa O(n)

def lengthOfListR[A](xs:List[A]):Int=
  def lengthOfListIter[A](xs:List[A],x:Int):Int=
    if xs==Nil then x
    else lengthOfListIter(xs.tail,x+1)
  lengthOfListIter(xs,0)

lengthOfListR(List(1,2,3)) == 3
lengthOfListR(List('a','b','c','d')) == 4
lengthOfListR(List()) == 0

// złożoność obliczeniowa O(n)
// złożoność pamięciowa O(1)


//zadanie3
def joinLists[A](xs: List[A], ys: List[A]):List[A]=
  (xs,ys) match
    case (hx::tx,hy::ty) => hx::hy::joinLists(tx,ty)
    case (Nil,_) => ys
    case (_,Nil) => xs

joinLists (List(5,6), List(1,2,3)) == List(5, 1, 6, 2, 3)
joinLists ( List(1,2,3),List(5,6)) == List(1, 5, 2, 6, 3)
joinLists (List("Ala", "ma", "kota"),List(",")) == List("Ala",",", "ma", "kota")
joinLists(Nil,Nil) == List()

// złożoność obliczeniowa O(n)
// złożoność pamięciowa O(n)

