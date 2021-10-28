/**
 * @author Jakub Szwedowicz
 * @version 1.0
 *          date: 28.10.2021
 *          email: kuba.szwedowicz@gmail.com
 */

def reverse(l: List[Int]): List[Int] =
  if l == Nil then Nil
  else reverse(l.tail) ::: List(l.head)

// Task 1
def splitBySign(l: List[Int]): (List[Int], List[Int]) =
  def split(l: List[Int], negative: List[Int], positive: List[Int]): (List[Int], List[Int]) = l match
    case Nil =>
      (negative, positive)
    //      if l.head % 2 != 0 then
    case (head :: tail) =>
      if head < 0 then split(l.tail, List(head) ::: negative, positive)
      else if head > 0 && (head % 2 != 0) then split(l.tail, negative, List(head) ::: positive)
      else split(l.tail, negative, positive)

  val r = split(l, List(), List())
  (reverse(r._1), reverse(r._2))

val l1 = List(-3, -6, 7, -9, 13, 14)
val l2 = List(-3, -6)
val l3 = List()

println(splitBySign(l1) == (List(-3, -6, -9), List(7, 13)));
println(splitBySign(l2) == (List(-3, -6), List()));
println(splitBySign(l3) == (List(), List()));


// Task 2
def lengthOfList[A](l: List[A]): Int = l match
  case Nil => 0
  case _ => 1 + lengthOfList(l.tail)

println(lengthOfList(l1) == l1.length);
println(lengthOfList(l2) == l2.length);
println(lengthOfList(l3) == l3.length);


// Task 3
def joinLists[A](l1: List[A], l2: List[A]): List[A] =
  def join[A](first: List[A], second: List[A], res: List[A]): List[A] = (first, second) match
    case (h::t, _) => List(h) ::: join(second, t, res)
    case (Nil, _) => second

  join(l1, l2, List())

println(joinLists(l1, l2) == List(-3, -3, -6, -6, 7, -9, 13, 14));
println(joinLists(l2, l3) == List(-3, -6))
println(joinLists(l3, l3) == List())

