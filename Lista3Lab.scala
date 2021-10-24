package Lista3Lab

object Lista3Lab {

  def reverse[T](list: List[T]): List[T] = {
    def reverseRec[T](list: List[T], newList: List[T] ): List[T] = {
      list match {
        case Nil => newList
        case h::t => reverseRec(t, h::newList)
      }

    }
    reverseRec(list, Nil)
  }


  def splitBySign(list: List[Int]): (List[Int], List[Int]) = {

    def splitBySignRec(l: List[Int], accNeg: List[Int], accPos: List[Int]): (List[Int], List[Int]) = {
      (l) match {
        case Nil => (reverse(accNeg),reverse(accPos))
        case h::t => if (h < 0) {
          splitBySignRec(t, h :: accNeg, accPos)
        }
        else splitBySignRec(l.tail, accNeg, h :: accPos);

      }
    }
    splitBySignRec(list, Nil, Nil);
  }


    def length[T](l: List[T]): Int = {
      if (l == Nil) {
        0
      } else 1 + length(l.tail)

    }

    def addLists[T](list1: List[T], list2: List[T]): List[T] = {
      (list1, list2) match {
        case (Nil, _) => list2
        case (_, Nil) => list1
        case (h :: t, h2 :: t2) => h :: h2 :: addLists(t, t2)
      }

    }

    def main(args: Array[String]): Unit = {
      println(splitBySign(List(1,2,3,-1,-2,3,-2)));
      println(splitBySign(Nil));
      println(splitBySign(List(1,2,3,4,5)));
      println(splitBySign(List(-1,-2,-3,-6)));

      println(length(Nil)==0);
      println(length(List(1,2,3,4,5))==5);
      println(length(List(1))==1);
      println(length(List(-1,-2,-3,-4))==4);
      println(addLists(Nil,Nil));
      println(addLists(Nil, List(1,2,3,4)));
      println(addLists(List(1,2,3,4), Nil));
      println(addLists(List(1,2,3), List(6,5,4)));




    }
  }
