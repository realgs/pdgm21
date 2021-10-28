import scala.annotation
import scala.annotation.tailrec

//Zad 1

//metoda pomocnicza reverse
def reverse(list: List[Int]): List[Int] =
  @tailrec
  def reverse(list: List[Int], reversed: List[Int]): List[Int] =
    list match
      case Nil => reversed
      case head :: tail => reverse(list.tail, head :: reversed)

  reverse(list, Nil)

//metoda spliBySign
def splitBySign(list: List[Int]): (List[Int], List[Int]) =
  @tailrec
  def splitBySign(list: List[Int], lower: List[Int], higher: List[Int]): (List[Int], List[Int]) =
    list match
      case Nil => (reverse(lower), reverse(higher))
      case head :: tail =>
        if (head > 0 && head % 2 != 0) then splitBySign(tail, lower, head :: higher)
        else if head < 0 then splitBySign(tail, head :: lower, higher)
        else splitBySign(tail, lower, higher)

  splitBySign(list, Nil, Nil)

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(-1, 2, 7, -8, 2, 3)) == (List(-1, -8), List(7, 3))
splitBySign(List(-1, 4, 11, -80, 2, 14, 27, 19, -11)) == (List(-1, -80, -11), List(11, 27, 19))
splitBySign(List(-1)) == (List(-1), List())

//Zad 2

def lengthOfList[A](list: List[A]): Int =
  if list == Nil then 0
  else 1 + lengthOfList(list.tail)

lengthOfList(List(5, 4, 3, 2)) == 4;
lengthOfList(List(-1, 2, 7, -8, 2, 3)) == 6;
lengthOfList(List(-1, 4, 11, -80, 2, 14, 27, 19, -11)) == 9;
lengthOfList(List(0)) == 1;

//Zad 3

def joinLists[A](list1: List[A], list2: List[A]): List[A] =
  (list1, list2) match
    case (Nil, Nil) => Nil
    case (Nil, head :: tail) => head :: joinLists(Nil, tail)
    case (head :: tail, Nil) => head :: joinLists(tail, Nil)
    case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: joinLists(tail1, tail2)

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List("Panstwo", "bezprawnie", "tereny", "do"), List("Izrael", "okupuje", "nalezace", "Palestyny")) == List("Panstwo", "Izrael", "bezprawnie", "okupuje", "tereny", "nalezace", "do", "Palestyny")
joinLists(List(1, 2, 3, 4), List()) == List(1, 2, 3, 4)

