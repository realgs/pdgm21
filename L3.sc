def splitBySign(xs: List[Int]) : (List[Int], List[Int]) =
  xs match {
    case head::body => 
      val result = splitBySign(body)
      if head < 0 then  
        (result._1, head::result._2)
      else if head % 2 == 1 then
        (head::result._1, result._2)
      else
        result
    case Nil => 
      (List(), List())
  }
  

splitBySign(List(1,2,-3,4,-5,-6,7,8)) == (List(1, 7),List(-3, -5, -6))
splitBySign(List()) == (List(),List())
splitBySign(List(-2, -4, -5)) == (List(),List(-2, -4, -5))
splitBySign(List(0, 1, 2)) == (List(1),List())


def lengthOfList[A](xs: List[A]) : Int =
  def accLengthOfList(list: List[A], acc: Int) : Int =
    list match {
      case head::body => accLengthOfList(body, acc+1)
      case Nil => acc
    }
  accLengthOfList(xs, 0)

lengthOfList(List(0,1,2,3,4,5)) == 6
lengthOfList(List(1,1,1,1,1)) == 5
lengthOfList(List("a","b","c","d","e", "f", "g")) == 7
lengthOfList(List()) ==  0
lengthOfList(List(3.0)) == 1
  

def joinLists[A](l1: List[A], l2: List[A]) : List[A] =
  (l1, l2) match
    case (h1 :: b1, h2 :: b2) => h1 :: h2 :: joinLists(b1, b2)
    case (h1 :: b1, Nil) => l1
    case (Nil, h2 :: b2) => l2
    case _ => Nil
  

joinLists(List(1, 3, 5, 7), List(2,4,6,8)) == List(1, 2, 3, 4, 5, 6, 7, 8)
joinLists(List(), List(2,4,6,8)) == List(2, 4, 6, 8)
joinLists(List(1, 3, 5, 7), List()) == List(1, 3, 5, 7)




