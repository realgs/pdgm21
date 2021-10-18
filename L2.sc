//Sebastian Bednarski

//task 1

val sum: List[Int] => Int = x =>
  if(x==Nil) then 0
  else x.head + sum(x.tail)

sum(List(1, 2, 3, 4, 5)) == 15
sum(List(-1, 2, 3 )) == 4
sum(List()) == 0
sum(List(5.6.toInt, 5)) == 10
sum(List(1, 5, 8)) == 14

//task 2

def listofstr[A] (x:List[A]) : String =
  if(x.tail == List()) then s"${x.head}!"
  else s"${x.head} ${listofstr(x.tail)}"

listofstr(List("Ala", "ma", "kota")) == "Ala ma kota!"
listofstr(List(1, "vs", 2)) == "1 vs 2!"
listofstr(List(""))

//task 3
val morezero: List[Int] => Boolean = x =>
  if(x == List()) then true
  else if(x.head <= 0) then false
  else morezero(x.tail)


morezero(List(1, 2, 3, 4))
morezero(List(0))
morezero(List(0, 0, 0, 0, 0))
morezero(List())

//task 4
def silnia (n:Int): Int =
  if(n == 0) then 1
  else if(n<0) then throw new Exception(s"ujemny argument $n")
  else n*silnia(n-1)

silnia(3) == 6
silnia(5) == 120
silnia(0) == 1
//silnia(-3)
