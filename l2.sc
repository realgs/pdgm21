//1
def sumOfElements(xs : List[Int]) : Int =
	if xs == Nil then 0
	else xs.head + sumOfElements(xs.tail)
	
sumOfElements(List(1,2,3,4,5,6,7,8,9)) == 45
sumOfElements(List()) == 0
sumOfElements(List(1,-1,2,-2,3,-3,4,-4)) == 0

//2
def sentence (xs : List[String], ch : Char): String =
	if xs == Nil then ch.toString
	else xs.head + " " + sentence(xs.tail, ch)

sentence(List("Ala", "ma", "kota"),'!') == "Ala ma kota !"
sentence(List(),'?') == "?" 	
sentence(List("","","","",""),'.') == "     ."

//3
def positive (xs: List[Int]): Boolean =
	if xs == Nil then true
	else 
		if xs.head < 0 then false
		else positive(xs.tail)

positive(List()) == true
positive(List(1,2,3)) == true
positive(List(1,2,-1)) == false

//4
def factorial(n:Int): Int =
	if n==0 then 1
	else if n>0 then n*factorial(n-1)
	else throw new Exception(s"Nonnegative number expected! $n")

factorial(5) == 120
factorial(0) == 1
//factorial(-2) 
