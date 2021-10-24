import scala.annotation.tailrec

//Zadanie 1
def twoListCreator(xs: List[Int]): (List[Int], List[Int]) =
  @tailrec
  def recTwoListCreator(xs: List[Int], as: List[Int], bs: List[Int]): (List[Int], List[Int]) =
    if xs == Nil then (as, bs)
    else if xs.head < 0 then recTwoListCreator(xs.tail, as ::: List(xs.head), bs)
    else if xs.head % 2 == 1 then recTwoListCreator(xs.tail, as, bs ::: List(xs.head))
    else recTwoListCreator(xs.tail, as, bs)

  recTwoListCreator(xs, Nil, Nil)

twoListCreator(List(-3, -6, 7, -9, 13))
twoListCreator(List(-3, -6, 7, -9, 13, 4, 9))

//Zadanie 2
def lengthOfList[A](xs: List[A]): Int =
  if xs == Nil then 0
  else 1 + lengthOfList(xs.tail)

lengthOfList(List("2", "1", "3", "7", "4", "2", "0")) == 7
lengthOfList(List()) == 0

//Zadanie 3

def zip[A](xs: List[A], ys: List[A]): List[A] =
  @tailrec
  def recZip(xs: List[A], ys: List[A], resultList: List[A]): List[A] =
    if xs == Nil then resultList ::: ys
    else if ys == Nil then resultList ::: xs
    else recZip(ys, xs.tail, resultList ::: List(xs.head))

  recZip(xs, ys, Nil)

zip(List(1, 2, 3), List('a', 'b', 'c'))
zip(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))