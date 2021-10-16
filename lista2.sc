// Szymon Anikiej


// zadanie 1
val suma = (p:(Int, Int)) => p._1+p._2

//testy do 1
suma(1, 5)
suma(0, 0)
suma(-13, 8)
suma(-9, -9)


// zadanie 2
val zdanie = (p:(String, String)) => p._1+" "+p._2+"."

//testy do 2
zdanie("Ala ma", "kota")
zdanie("", "")
zdanie("K$1wsa798%as", "")


// zadanie 3
val dodatnie: List[Int] => Boolean = (li:List[Int]) =>
  if li == Nil then true
  else if li.head <= 0 then false
  else dodatnie(li.tail)

//testy do zad 3
dodatnie(List(1, 3, 2))
dodatnie(List(0, 4))
dodatnie(List(16, -9))
dodatnie(Nil)


// zadanie 4
val silnia: Int => Int = (x:Int) =>
  if x <= 1 then 1        // funkcja zwróci wartość 1 gdy argument jest nieprawidłowy
  else x*silnia(x-1)

//testy do zad 4
silnia(5)
silnia(1)
silnia(0)
silnia(-4)
