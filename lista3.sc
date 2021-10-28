/// Dawid Krutul
/// Zadanie 1

def splitBySign[A](numbers: List[Int], list1: List[Int], list2: List[Int]): (List[Int], List[Int]) =
    if numbers != Nil then(numbers.head < 0, numbers.head % 2 != 0) match
      case(true,_) => splitBySign(numbers.tail,list1 ::: List(numbers.head),list2)
      case(false,true) => splitBySign(numbers.tail,list1,list2 ::: List(numbers.head))
      case(_,_) => splitBySign(numbers.tail,list1,list2)
    else
      (list1,list2)

splitBySign(List(-3,-6,7,-9,13),List(),List()) == (List(-3,-6,-9),List(7,13))
splitBySign(List(-2,-1,0,1,2,3,4),List(),List()) == (List(-2,-1),List(1,3))
splitBySign(List(),List(),List()) == (List(),List())


/// Zadanie 2

def lengthOfList[A](numbers: List[A]): Int =
  if(numbers == Nil) 0
  else lengthOfList(numbers.tail) + 1

lengthOfList(List(1,2,3,4)) == 4
lengthOfList(List()) == 0
lengthOfList(List("jest","fajnie")) == 2

/// Zadanie 3

def joinLists[A](list1: List[A], list2: List[A]): List[A] =
  (list1,list2) match
    case(Nil,Nil) => Nil
    case(Nil,_) => list1.head :: joinLists(list1,list2.tail)
    case(_,Nil) => list1.head :: joinLists(list1.tail,list2)
    case(_,_) => list1.head :: list2.head :: joinLists(list1.tail,list2.tail)

joinLists(List(1,3,5,7),List(2,4,6,8)) == List(1,2,3,4,5,6,7,8)
joinLists(List(1,2,3,4),List(1,2,3,4)) == List(1,1,2,2,3,3,4,4)
joinLists(List(),List()) == List()




