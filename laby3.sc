
//zadanie 1
def positiveAndOdd(xs: List[Int]): List[Int] = {
  if xs == Nil then List()
  else if xs.head > 0 && xs.head % 2 == 1 then xs.head :: positiveAndOdd(xs.tail)
  else Nil ::: positiveAndOdd(xs.tail)
}

def negative(xs: List[Int]): List[Int] = {
  if xs == Nil then List()
  else if xs.head < 0 then xs.head :: negative(xs.tail)
  else Nil ::: negative(xs.tail)
}

def splitBySign(xs: List[Int]): (List[Int], List[Int]) = {
  if xs == Nil then (List(), List())
  else (negative(xs), positiveAndOdd(xs))
}
//complexity = O(n), n = xs.length

//tests
splitBySign(List(-3, -6, 7, -9, 13))
splitBySign(List(-1, -2, 0, 1, 2, 3, 4))
splitBySign(List(-2, 3, 1, 0, 2, 4, -1))


//zadanie 2
def lengthOfList[A](xs: List[A]): Int = {
  if xs == Nil then 0
  else 1 + lengthOfList(xs.tail)
}
//complexity = O(n), n = xs.length

//tests
lengthOfList(List(5, 4, 3, 2))
lengthOfList(List())
lengthOfList(List(1))
lengthOfList(List(1, 2, 3, 4))
lengthOfList(List('a', 'b', 'c'))
lengthOfList(List(true, false, true))


//zadanie 3
def joinLists[A](as: List[A], bs: List[A]): List[A] = {
  if as == Nil && bs == Nil then Nil
  else if as == Nil && bs != Nil then
    bs.head :: joinLists(Nil, bs.tail)
  else if as != Nil && bs == Nil then
    as.head :: joinLists(as.tail, Nil)
  else as.head :: bs.head :: joinLists(as.tail, bs.tail)
}
//complexity = O(n), n = max(as.length, bs.length)

//tests
joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6))
joinLists(List('a', 'a', 'a'), List('b', 'b', 'b', 'c'))
joinLists(List('a', 'a', 'a', 'a', 'd'), List('b', 'b', 'b'))
joinLists(List(), List(1, 2, 3, 4, 5, 6))
joinLists(List(1, 2, 3, 4, 5), List())