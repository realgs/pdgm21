def splitBySign(xs: List[Int]): (List[Int], List[Int]) = {
  def splitBySignIn(xs: List[Int], neg_list: List[Int], pos_list: List[Int]): (List[Int], List[Int]) = {
    if xs == Nil then (neg_list, pos_list)
    else if xs.head < 0 then splitBySignIn(xs.tail, xs.head :: neg_list, pos_list)
    else if xs.head > 0 && xs.head % 2 != 0 then splitBySignIn(xs.tail, neg_list, xs.head :: pos_list)
    else splitBySignIn(xs.tail, neg_list, pos_list)
  }

  splitBySignIn(xs, List(), List())
}

println(splitBySign(List(-3, -6, 7, -9, 13)))
println(splitBySign(Nil))
println(splitBySign(List(1, 2, 3, 4, 5, -6, -7, -8, -9, -10)))


def lengthOfList[A](list: List[A]): Int =
  if list == Nil then 0 else 1 + lengthOfList(list.tail)


println(lengthOfList(List(0, 0, 0, 0)))
println(lengthOfList(List("a", "b", "c")))
println(lengthOfList(List()))



def joinLists[A](list1: List[A], list2: List[A]): List[A] = {
  (list1, list2) match
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (_,_) => list1.head :: list2.head :: joinLists(list1.tail, list2.tail)
}

println(joinLists(List(1, 3, 5, 7), List(2, 4, 6, 8)))
println(joinLists(Nil, Nil))
