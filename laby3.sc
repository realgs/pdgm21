
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
