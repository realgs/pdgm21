import sun.font.TrueTypeFont

def zad1(l:List[Int]): Int =
  if l != Nil then
    l.head + zad1(l.tail)
  else 0

zad1(List()) == 0
zad1(List(1, 2, 0, 3, 4)) == 10
zad1(List(1, -1, 2 , -2)) == 0

//helper zad2
def listLength[A](list: List[A]): Int =
  if (list != Nil) then
    1 + listLength(list.tail)
  else 0

def zad2(l: List[String], c: String): String =
    if listLength(l) == 0 then c
    else if listLength(l) == 1 then l.head + c
    else l.head + "_" + zad2(l.tail, c)


zad2(List(),"!") == "!"
zad2(List("Tom", "i", "mysz"), "!") == "Tom_i_mysz!"
zad2(List("Musztarda"), "!") == "Musztarda!"

def zad3(l: List[Int]): Boolean =
    if l == Nil then true
    else if l.head > 0 then zad3(l.tail)
    else false

zad3(List(3, 4, 23, 78)) == true
zad3(List(0, 5, 12)) == false
zad3(List(9, 10, -1)) == false
zad3(List()) == true

def zad4(n: Int): Int =
    if n<0 then throw new Exception("Bad number")
    else if n==0 then 1
    else if n==1 then 1
    else n*zad4(n-1)

zad4(5) == 120
zad4(0) == 1
zad4(1) == 1
zad4(-1)
