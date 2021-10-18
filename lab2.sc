
//zadanie 1
val suma: List[Int] => Int = xs =>{
  if xs != Nil then xs.head + suma(xs.tail) else 0
}

suma(List(1,2,3,4))
suma(List(3))
suma(List())
suma(List(1,2,-3,0))

//zadanie 2
val separator : String = " "

val zdanie: (List[String], Char) => String = (xs, k) => {
if xs != Nil then xs.head + separator + zdanie(xs.tail, k)
else k.toString
}

zdanie(List("poczatek","koniec"), '!')
zdanie(List("wyraz_a","wyraz_b","wyraz_c"), '.')
zdanie(List(), '?')

//zadanie 3
 val dodatnie: List[Int] => Boolean = xs => {
   if xs != Nil then
   xs.head > 0 && dodatnie(xs.tail)
   else true
 }

dodatnie(List(1,2,3))
dodatnie(List(1))
dodatnie(List())
dodatnie(List(1,2,-1))
dodatnie(List(0,0,0))
dodatnie(List(-1,-2,-3))

//zadanie 4
val silnia: Int => Int = n => {
  if n > 0then n * silnia(n-1)
  else if n==0 then 1
  else throw new Exception("Ujemny argument")
}

silnia(10)
silnia(1)
silnia(0)
silnia(-1)
