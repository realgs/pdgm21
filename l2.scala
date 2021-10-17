//Paulina Drzazga, 260370

//Task 1

def sum(ls:List[Int]):Int={
  if ls==Nil then 0
  else ls.head+sum(ls.tail)
}
sum(List(10,15))==25
sum(List(-100, 1000,345,46,23,43,-34))==1323
sum(List())==0

//Task 2

val sep=" "

def sentence(ls: List[String], char: String):String={
  if ls==Nil then char
  else ls.head+sep+sentence(ls.tail, char)
}

sentence(List("My", "name", "is", "Paulina"), ".")=="My name is Paulina ."
sentence(List(), "!")=="!"
sentence(List("I", "love", "programming"), "")=="I love programming "

//Task 3

def positiveArguments(ls:List[Int]):Boolean= {
  if ls == Nil then true
  else {
    if ls.head > 0 then positiveArguments(ls.tail)
    else false
  }
}

positiveArguments(List(1,2,3,4,5,6,7,8,9,10))==true
positiveArguments(List(1,2,3,4,5,6,7,8,9,-10))==false
positiveArguments(List())==true
positiveArguments(List(-1,-2,-3,4,5,6,7,8,9,10))==false

//Task 4

def factorial(n:Int):Int={
  if n<0 then throw new Exception("Negative argument")
  else if n==0 then 1
  else factorial(n-1)*n
}

factorial(5)==120
factorial(6)==720
factorial(0)==1
//factorial(-10)        //EXCEPTION
