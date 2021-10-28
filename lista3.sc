import scala.annotation.tailrec

def reverse [A](lista: List[A]): List[A] =
  @tailrec
  def rev (lista:List[A], wynik:List[A]): List[A] =
    if lista == Nil then wynik else rev (lista.tail, lista.head::wynik)
  rev(lista, Nil)

// zad 1
def splitBySign (lista:List[Int]): (List[Int], List[Int]) =
  @tailrec
  def splitIter (lista:List[Int], neg:List[Int], pos:List[Int]): (List[Int], List[Int]) =
    lista match
      case Nil => (neg, pos)
      case h::t => if h < 0 then splitIter(t, h::neg, pos) else
        if h > 0 && h % 2 == 1 then splitIter(t, neg, h::pos) else
          splitIter(t, neg, pos)
  splitIter(reverse(lista), Nil, Nil)

// testy do 1
splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(Nil) == (Nil, Nil)
splitBySign((List(0, -1, 1, 1, -1, 0, 2, -2, -2, 2, 0))) == (List(-1, -1, -2, -2), List(1, 1))

// zad 2
def lengthOfList [A](lista:List[A]): Int =
  @tailrec
  def len(lista:List[A], n:Int): Int =
    if lista == Nil then n else len(lista.tail, n+1)
  len(lista, 0)

// testy do 2
lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(Nil) == 0
lengthOfList(List("Koperek")) == 1;;

// zad 3
def joinLists [A](listaL:List[A], listaP:List[A]): List[A] =
  @tailrec
  def join (listaL:List[A], listaP:List[A], wynik:List[A], np:Boolean): List[A] =
    (listaL, listaP, np) match
      case (Nil, Nil, _) => reverse(wynik)
      case (h1::t1, Nil, _) => join(t1, listaP, h1::wynik, false)
      case (h1::t1, h2::t2, true) => join(t1, listaP, h1::wynik, false)
      case (Nil, h2::t2, _) => join(listaL, t2, h2::wynik, true)
      case (h1::t1, h2::t2, false) => join(listaL, t2, h2::wynik, true)
  join(listaL, listaP, Nil, true)

// testy do 3
joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(List(3.2, 5.6, 1.1), Nil) == List(3.2, 5.6, 1.1)
joinLists(Nil, List("Pomysłowe słowo 1", "Pomysłowe słowo 2", "Pomysłowe słowo 3")) == List("Pomysłowe słowo 1", "Pomysłowe słowo 2", "Pomysłowe słowo 3")
joinLists(Nil, Nil) == Nil
