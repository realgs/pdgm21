// zadanie 1
def sum(xs: List[Int]):Int = {
 if xs != Nil then xs.head + sum(xs.tail)
 else 0
}
sum(Nil)
sum(List(1,2,3,4,5))

//zadanie 2
def str(xs: List[String],x:String):String = {
  if xs != Nil then xs.head ++ " " ++ str(xs.tail,x)
  else x
}

str(List("Ala","ma","Kota"),".")
str(List(),"")

//zadanie 3
def more_then_0(xs: List[Int]):Boolean = {
 if xs != Nil then {
  if xs.head > 0 then more_then_0(xs.tail)
  else false
 }
 else true
}

more_then_0(List())
more_then_0(List(1,2,3,0))
more_then_0(List(0,2,3,4))
more_then_0(List(1,2,3,5))

//zadanie 4
def silnia(x: Int):Int = {
 if x == 0 then 1 else x*silnia(x-1)
}
silnia(0)
silnia(1)
silnia(10)
