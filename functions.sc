def splitBySign(xs: List[Int]): (List[Int], List[Int]) =
    def splitBySignIter(xs: List[Int], pos: List[Int], neg: List[Int]): (List[Int], List[Int]) =
        xs match {
            case List() => (neg, pos)
            case h::t =>
                if h < 0 then splitBySignIter(t, pos, neg ::: List(h))
                else if h % 2 != 0 then splitBySignIter(t, pos ::: List(h), neg)
                else splitBySignIter(t, pos, neg)
        }
    splitBySignIter(xs, List(), List())

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) == (List(-5, -4, -3, -2, -1), List(1, 3, 5))
splitBySign(List()) == (List(), List())

def lengthOfList[A](xs: List[A]): Int =
    def lengthOfListIter(xs: List[A], acc: Int): Int =
        if xs == List() then acc
        else lengthOfListIter(xs.tail, acc + 1)
    lengthOfListIter(xs, 0)

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == 9
lengthOfList(List()) == 0

def joinLists[A](xs: List[A], ys: List[A]): List[A] =
    def joinListsIter(xs: List[A], ys: List[A], result: List[A]): List[A] =
        (xs, ys) match {
            case (List(), List()) => result
            case (List(), ys) => result ::: ys
            case (xs, List()) => result ::: xs
            case (h1::t1, h2::t2) => joinListsIter(t1, t2, result ::: List(h1, h2))
        }
    joinListsIter(xs, ys, List())

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(), List(1, 2, 3)) == List(1, 2, 3)
joinLists(List(1, 2, 3), List()) == List(1, 2, 3)
joinLists(List(), List()) == List()
joinLists(List(1), List(1)) == List(1, 1)
