object l3 {

  //zadanie 1
  def splitBySign (list: List[Int]): (List[Int],List[Int]) =
    list match
      case Nil => (Nil, Nil)
      case head::tail => {
        val(negative, positiveOdd) = splitBySign(tail)
        if head < 0 then (head::negative, positiveOdd)
        else if head %2 == 1 then (negative, head::positiveOdd)
        else (negative, positiveOdd)
      }

  //zadanie 2
  def lengthOfList[A](list :List[A]):Int =
    if list == Nil then 0
    else 1 + lengthOfList(list.tail)

  //zadanie 3
  def joinList [A](list1: List[A], list2: List[A]): List[A] =
    (list1,list2) match {
      case (Nil, Nil) => Nil
      case (Nil,_) => list2
      case (_,Nil) => list1
      case (head1::tail1, head2::tail2) => head1::head2::joinList(tail1,tail2);;
    }


  def main(args: Array[String]): Unit = {

    //testy zadanie 1
    println(splitBySign(List(-3,-6,7,-9,13)) == (List(-3, -6, -9),List(7, 13)));
    println(splitBySign(Nil) == (List(),List()));
    println(splitBySign(List(1,2,-3)) == (List(-3),List(1)));
    println(splitBySign(List(1,3)) == (List(),List(1, 3)));

    //testy zadanie 2
    println(lengthOfList(Nil) == 0);
    println(lengthOfList(List(1,2)) == 2);
    println(lengthOfList(List("a","b","c")) == 3);
    println(lengthOfList(List(5,4,3,2)) == 4);

    //testy zadanie 3
    println(joinList(List(5,4,3,2),List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6));
    println(joinList(List(),List(1,2,3)) == List(1,2,3));
    println(joinList(List("a","b"),List("c","d")) == List("a","c","b","d"));
    println(joinList(List(1,3,5,7),List(2,4)) == List(1, 2, 3, 4, 5, 7));
  }

}
