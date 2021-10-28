//zadanie1

def splitBySign (xs: List[Int]): (List[Int], List[Int]) =

  xs match
    case (h :: t) => {
      val(small, large) = splitBySign(xs.tail)
      if h < 0 then
        (h :: small, large)
      else if h > 0 && h%2 != 0 then
        (small, h :: large)
      else  splitBySign(t)
    }
    case _ => (Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13))
splitBySign(List(1,2,0,3,-4))

//zlozonosc obliczeniowa n

//zadanie2

def lengthOfList[A](xs: List[A]): Int =
  xs match
    case (h :: t) => 1 + lengthOfList(t)
    case _ => 0

lengthOfList(List(1,2,3,4)) == 4
lengthOfList(List()) == 0
lengthOfList(List(1,2,"ala", "ma", "kota")) == 5

//zlozonosc oblczeniowa n

//zadanie3

def joinLists[A](xs1: List[A], xs2: List[A]): List[A]=
  (xs1,xs2) match
    case ((h1 :: t1), (h2 :: t2)) => h1 :: h2 :: joinLists(t1,t2)
    case (Nil, (h2::t2)) => h2 :: joinLists(Nil, t2)
    case ((h1::t1), Nil) => h1 :: joinLists(t1, Nil)
    case _ => Nil

joinLists(List(5,4,3,2), List(1,2,3,4,5,6))
joinLists(List(), List())
joinLists(List(1,2,3),List())

//zlozonosc oblcizeniowa n
