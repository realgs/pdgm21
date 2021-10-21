/* Stanisław Izdebski */

/* Zad.1 */

val Add: (xs:List[Int])=> Int= xs =>
{
  if xs==Nil then 0
  else
    if xs.tail == Nil then xs.head
    else Add(xs.tail)
}
Add(List(1,-2,-3,4,5))
Add(List(5))
Add(Nil)

/* Zad.2 */

val StringUniter: (xs:List[String],s: String)=> String= (xs,s) =>
{
  if xs==Nil then ""
  else
    if s!="." && s!="!" && s!="?" then throw new Exception("Niepoprawny znak konczacy zdanie")
    else
       if xs.tail == Nil then xs.head+s
       else xs.head+" "+StringUniter(xs.tail,s)
}
StringUniter(List("One", "Ring", "to", "rule", "them", "all,", "One" ,"Ring" ,"to" ,"find" ,"them,","One" ,"Ring", "to", "bring" ,"them" ,"all", "and" ,"in", "the", "darkness", "bind" ,"them"),".")
StringUniter(Nil,".")
//StringUniter(List("Something"),"g")


/* Zad.3 */

val AboveZeroChecker: (xs:List[Int])=> Boolean= xs =>
{
  if xs==Nil then throw new Exception("List jest pusta")
  else
    if xs.head <= 0 then false
    else
      if xs.tail == Nil then true
      else AboveZeroChecker(xs.tail)
}
AboveZeroChecker(List(1,2,3,4,5,6))
AboveZeroChecker(List(1,2,3,4,0,6))
AboveZeroChecker(List(4,7,1,3,-5))
//AboveZeroChecker(Nil)

/* Zad.4 */

val Factorial: (n:Int)=> BigInt= n =>
{
  def FactorialCounter (n:Int,acc:BigInt):BigInt=
  {
    if n == 0 then acc
    else FactorialCounter(n - 1, acc * n)
  }
  if n<0 then throw new Exception("Ujemny argumet")
  else
    FactorialCounter(n,1)
}
Factorial(0)
Factorial(30)
//Factorial(-5)

