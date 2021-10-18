//zadanie 1
def suma(xs: List[Int]): Int =
  if xs==Nil then 0
  else xs.head + suma(xs.tail)

suma(List()) == 0
suma(List(1,2,3,4,5)) == 15
suma(List(-5,0,7,-3)) == -1

//zadanie 2
def zdanie(xs: List[String]): String =
  if xs==Nil then ""
  else if xs.head=="." then xs.head
  else if xs.tail==List(".") then xs.head + xs.tail.head
  else xs.head + " " + zdanie(xs.tail)

zdanie(List()) == ""
zdanie(List("Ala","ma","kota",".")) == "Ala ma kota."
zdanie(List("xyz",".")) == "xyz."
zdanie(List(".")) == "."

//zadanie 3
def czyDodatnie(xs: List[Int]): Boolean =
  if xs==Nil then true
  else if xs.head<=0 then false
  else czyDodatnie(xs.tail)

czyDodatnie(List(1,2,3))
czyDodatnie(List())
czyDodatnie(List(-1, 2, -3)) == false
czyDodatnie(List(5,5,0)) == false

//zadanie 4
def silnia(n: Int): Int =
  if n==0 then 1
  else if n>0 then n*silnia(n-1)
  else throw new Exception("ujemny argument")

silnia(0) == 1
silnia(1) == 1
silnia(5) == 120
//silnia(-1)  //Exception

