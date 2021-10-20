//Zadanie 1
def sum(xs: List[Int]): Int =
  if xs == Nil then 0
  else xs.head + sum(xs.tail)

sum(List(1, 4, 6, 2))
sum(List())
sum(List(-2, -4, 5, 3))

//Zadanie 2
val space = " "
def writer(xs: List[String], x: String): String =
  if xs == Nil then x
  else xs.head + {
    if xs.tail == Nil then writer(xs.tail, x)
    else space + writer(xs.tail, x)
  }

writer(List("Ala", "ma", "kota"), ".")

//Zadanie 3
def bigerThanZero(xs: List[Int]): Boolean =
  if xs == Nil then true
  else if xs.head < 0 then false
  else bigerThanZero(xs.tail)

bigerThanZero(List(1, 5, 8, 3))
bigerThanZero(List(1, 5, -8, 3))
bigerThanZero(List())

//Zadanie 4
def silnia(n: Int): Int =
  if n == 0 then 1
  else if n > 0 then n * silnia(n - 1)
  else throw new Exception("Ujemna wartosc warunku! (n < 0)")

silnia(4)
silnia(0)
silnia(-2)
