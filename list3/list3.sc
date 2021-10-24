import scala.annotation.tailrec

// Zadanie 1
// z³o¿onoœæ czasowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
// z³o¿onoœæ pamieciowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
def splitBySign(list: List[Int]): (List[Int], List[Int]) =
    if list == Nil then
        return (Nil, Nil)
    else
        val (lower, higher) = splitBySign(list.tail)
        if list.head < 0 then
            return (list.head :: lower, higher)
        else if list.head % 2 == 1 then
            return (lower, list.head :: higher)
        else
            return (lower, higher)

// Zadanie 2
// z³o¿onoœæ czasowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
// z³o¿onoœæ pamiêciowa sta³a O(1)
def listLength[T](list: List[T]): Int =
    @tailrec
    def listLengthTail(list: List[T], length: Int): Int =
        if (list == Nil) length
        else listLengthTail(list.tail, 1 + length)

    return listLengthTail(list, 0)


// Zadanie 3
// z³o¿onoœæ czasowa liniowa O(n), gdzie n jest d³ugoœci¹ listy wejœciowej
// z³o¿onoœæ pamiêciowa liniowa O(n), gdzie n jest d³ugoœci¹ krótszej listy wejœciowej
def joinLists[T](listA: List[T], listB: List[T]): List[T] =
    (listA, listB) match
        case (Nil, _) => listB
        case (_, Nil) => listA
        case (hd1 :: tl1, hd2 :: tl2) =>
            hd1 :: hd2 :: joinLists(tl1, tl2)



splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(1, 2, -1, -2, -3, 3, 4, -4, -5, 5))
splitBySign(List())

listLength(List(5, 4, 3, 2)) == 4
listLength(List(List("Ala", "ma"), List("kota")))
listLength(List())

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(9, 8, 7), List(1, 2, 3, 4, 5, 6))
joinLists(List(), List(1, 2, 3, 4, 5, 6))
joinLists(List(), List())
joinLists(List(5, 3, 4, 5, 6), List())
