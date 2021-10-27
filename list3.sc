import scala.annotation.tailrec
//zad1


def reverseList[A](list : List[A]) : List[A] ={
  @tailrec
  def reverseIter(listToReverse : List[A], reversed : List[A]) : List[A] ={
    listToReverse match {
      case Nil => reversed
      case hd :: tl => reverseIter(tl, hd :: reversed)
    }
  }
  reverseIter(list, Nil)
}

def splitBySign(list : List[Int]) : (List[Int], List[Int]) = {
  @tailrec
  def split(listToSplit: List[Int], splitNeg: List[Int], splitPosOdd: List[Int]): (List[Int], List[Int]) = {
    listToSplit match {
      case Nil => (splitNeg, splitPosOdd)
      case hd :: tl =>
        if hd < 0 then split(tl, hd :: splitNeg, splitPosOdd)
        else if hd > 0 && hd % 2 == 1 then split(tl, splitNeg, hd :: splitPosOdd)
        else split(tl, splitNeg, splitPosOdd)
    }
  }
  split(reverseList(list), Nil, Nil)
}
splitBySign(List(-3,-6,7,-9,13)) == (List(-3,-6,-9), List(7,13))
splitBySign(List(-3,-6,0,-2,20, 17)) == (List(-3,-6,-2), List(17))
splitBySign(List(-3,-6,0,-2,20, 18)) == (List(-3,-6,-2), List())
splitBySign(Nil) == (Nil, Nil)

//obliczeniowa O(n)
//pamięciowa O(n)

//zad2
def lengthOfList[A](list : List[A]) : Int ={
  @tailrec
  def lengthIter[A](acc : Int, tl : List[A]) : Int = {
    if tl == Nil then acc
    else lengthIter(acc +1, tl.tail)
  }
  lengthIter(0, list)
}

lengthOfList(List(5,4,3,2)) == 4
lengthOfList(List('a','b')) == 2
lengthOfList(Nil) == 0

//obliczeniowa O(n)
//pamięciowa O(1)

//zad3
def joinLists[A](list1 : List[A], list2 : List[A]) : List[A] = {
    (list1, list2) match {
      case (Nil, Nil) => Nil
      case (Nil, hd :: tl) => hd :: joinLists(Nil, tl)
      case (hd :: tl, Nil) => hd :: joinLists(tl, Nil)
      case (hd1 :: tl1, hd2 :: tl2) => hd1 :: hd2 :: joinLists(tl1, tl2)
    }
}

joinLists(List(5,4,3,2), List(1,2,3,4,5,6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List('a','a','a','o'), List('l','m','k','t','a')) == List('a','l', 'a', 'm', 'a', 'k', 'o', 't', 'a')
joinLists(Nil, List("Hell", "o", " ", "world")) == List("Hell", "o", " ", "world")
joinLists(Nil, Nil) == Nil

//obliczeniowa O(n)
//pamięciowa O(n)
