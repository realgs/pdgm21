def zad1(l:List[Int]): Int =
  if l != Nil then
    l.head + zad1(l.tail)
  else 0

zad1(List()) == 0
zad1(List(1, 2, 0, 3, 4)) == 10
zad1(List(1, -1, 2 , -2)) == 0

//helper zad2
def listLength[A](xs: List[A]): Int =
  if (xs != Nil) then
    1 + listLength(xs.tail)
  else 0

def zad2(l: List[String],x: String): String =
    if listLength(l) == 0 then x
    else if listLength(l) == 1 then l.head + x
    else l.head + "_" + zad2(l.tail, x)


zad2(List(),"!") == "!"
zad2(List("Tom", "i", "mysz"), "!") == "Tom_i_mysz!"
zad2(List("Musztarda"), "!") == "Musztarda!"