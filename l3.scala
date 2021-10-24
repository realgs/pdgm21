//Paulina Drzazga
//zadanie 1

def splitBySign (xs: List[Int]): (List[Int], List[Int]) =
  xs match
    case h::t => {
      val (neg, pozOdd) = splitBySign(t)
      if h < 0 then (h :: neg, pozOdd)
      else if h % 2 != 0 then (neg, h :: pozOdd)
      else (neg, pozOdd)
    }
    case Nil => (Nil, Nil)

splitBySign(List((-3),(-6),7,(-9),13))==(List(-3,-6,-9), List(7,13))
splitBySign(Nil)==(Nil, Nil)
splitBySign(List(1,2,3,4,5,6))==(Nil, List(1,3,5))

/*
Złożoność obliczeniowa: O(n) - n - długość listy
Złożoność pamięciowa: O(n) - n - długość listy
*/


//zadanie 2

def lengthOfList[A](xs:List[A]):Int=
  if xs==Nil then 0
  else  1+lengthOfList(xs.tail)

lengthOfList(List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))==8
lengthOfList(List(5, 1, 4, 3, 2))==5
lengthOfList(Nil)==0

/*
Złożoność obliczeniowa: O(n) - n - długość listy
Złożoność pamięciowa: O(n) - n - długość listy
*/

//zadanie 3

def joinLists[A](xs:List[A], ys:List[A]):List[A]=
  (xs,ys) match
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (hx::tx, hy::ty) => hx::(hy:: joinLists(tx, ty))

joinLists(List(5,4,3,2), List(1,2,3,4,5,6))==List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(), List(1,2,3,4,5,6))==List(1, 2, 3, 4, 5, 6)
joinLists(List(5,4,3,2), List(1))==List(5, 1, 4, 3, 2)
joinLists(List('a','c','e','g'), List('b','d','f','h'))==List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h')
joinLists(List(), List())==Nil

/*
Złożoność obliczeniowa: O(n) - n - długość krótszej listy
Złożoność pamięciowa: O(n) - n - długość krótszej listy
*/
