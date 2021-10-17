//Aleksandra Serwicka

//zadanie1
val sum: List[Int]=>Int = xs=>
  if xs==Nil then 0
  else xs.head + sum(xs.tail)

sum(List(1,2,3))==6
sum(List())==0
sum(List(1))==1



//zadanie2
val space=" "

val connectWords: (List[String],String)=>String = (xs,x)=>
  if xs==Nil then x
  else space+xs.head+connectWords(xs.tail,x)

println(connectWords(List("o","l","a"),"."))
println(connectWords(List(),"!"))
println(connectWords(List("ola","ma","kotka"),"."))

//zadanie3
val checkPositive: List[Int]=>Boolean = xs=>
  if xs==Nil then true
  else if xs.head > 0 then checkPositive(xs.tail)
  else false;

checkPositive(List(1,2,3))==true
checkPositive(List(1,2,-3))==false
checkPositive(List(-1,2,-3))==false
checkPositive(List())==true

//zadanie4

val factorial: Int=>Int = x=>
  if x<0 then throw new Exception("nie oblicze silni z ujemnej liczby")
  else if x==0 then 1
  else x * factorial(x-1)

factorial(3)==6
//factorial(-3) //exception
factorial(0)==1
