import scala.annotation.tailrec
//Mateusz Pietrych

//Zad 1

def sum (ns: List[Int]): Int =
  @tailrec
  def sumRec(as: List[Int], accum: Int): Int =
    if as == Nil then accum else sumRec(as.tail, accum+as.head)
  sumRec(ns,0)

sum(List(6,4,6,5,6))
sum(List(6,-5,6,5,-3))
sum(List())

//Zad 2

def listToString (ns: List[String], a: Char): String =
  @tailrec
  def listToStringRec (as: List[String], a: Char, accum: String): String =
    if as == Nil then accum + a else listToStringRec(as.tail, a, accum + " " + as.head)
  if ns != Nil then listToStringRec(ns.tail, a, ns.head) else ""


listToString(List("Ala","ma","kota"),'?')
listToString(List(),'?')
listToString(List(),' ')

//Zad 3

def listGreaterThan0 (ns: List[Int]): Boolean =
  if ns==Nil then true
  else if ns.head > 0 then listGreaterThan0(ns.tail)
  else false


listGreaterThan0(List(4,5,6,7,8))
listGreaterThan0(List(4,5,6,7,-9))
listGreaterThan0(List())

//Zad 4

def factorial (n: Int): Int =
  @tailrec
  def factorialRec(n: Int,accum: Int):  Int =
    if n==0 then accum else factorialRec(n-1,accum*n)
  factorialRec(n,1)

factorial(5)
factorial(0)
factorial(1)

