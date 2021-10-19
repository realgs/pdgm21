
def sum(xs:List[Int]):Int = {
  if xs == Nil then 0
  else xs.head + sum(xs.tail)
}

sum(List(1,2,3,4))
sum(List(-1,2))
sum(List())

val space = " "

def sentence(xs:List[String],s:String):String = {
  if xs == Nil then s
  else space +xs.head +sentence(xs.tail,s)
}

sentence(List("Nazywam","sie","Kuba"),".")
sentence(List("Ala","ma","Kota"),"!")
sentence(List(),"?")

def isPositive(xs:List[Int]):Boolean = {
  if xs==Nil then true
  else if xs.head<0 then false
  else isPositive(xs.tail)
}

isPositive(List(1,2,3))
isPositive(List(-1,2,3))
isPositive(List(-1,2,-3))

def factorial(n:Int):Int={
  if n<0 then throw new Exception("nie mozna obliczac silni z liczby ujemnej")
  else if n==0 then 1
  else n*factorial(n-1)
}
factorial(5)
factorial(17)
factorial(-1)
