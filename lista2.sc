//Justyna Stankiewicz

val separator=" "
val dot="."

/*Zadanie 1*/
def sum(xs:List[Int]):Int=
  if xs==Nil then 0
  else xs.head+sum(xs.tail)
sum(List(1,2,3,-4))==2
sum(List())==0
sum(List(1,2,3))==6

/*Zadanie 2*/
def sentence(xs:List[String]):String=
  if xs==Nil then ""
  else if xs.head=="?" then dot
  else separator+xs.head +sentence(xs.tail)
sentence(List("Ala","ma","kota","?"))==" Ala ma kota."
sentence(List())==""
sentence(List("Ala?","ma","kota","?"))==" Ala? ma kota."

/*Zadanie 3*/
def isPositive(xs:List[Double]):Boolean=
  if xs==Nil then false
  else if xs.head<=0 then false
  else
    if xs.tail==Nil then true
    else isPositive(xs.tail)

isPositive(List(2,2.4,3))==true
isPositive(List())==false
isPositive(List(-1,0,2))==false


/*Zadanie 4*/
def factorial(n:Int):Int=
  if n<0 then throw new Exception("blad")
  else if n==0 then 1
  else n*factorial(n-1)

factorial(4)==24
factorial(0)==1
//factorial(-2) zostanie wyrzucony blad

