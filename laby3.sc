
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


//zadanie 2
def lengthOfList[A](xs: List[A]): Int = {
  if xs == Nil then 0
  else 1 + lengthOfList(xs.tail)
}
//complexity = O(n), n = xs.length



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
