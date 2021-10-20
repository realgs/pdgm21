def sum(xs: List[Int]): Int =
  if xs == List() then 0 else xs.head + sum(xs.tail)

sum(List(1, 2, 3, 4)) == 10
sum(List()) == 0
sum(List(1, -1, 2)) == 2
