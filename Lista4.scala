package packMain
import scala.annotation.tailrec
object Lista4 {
  def main(args: Array[String]): Unit = {
    print("\n\n")

    println("Testy do zadania 1:")
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("iind", "oplp")))
    println(find(List("Ala", "ma", "kota", "pieska", "mitsita", "oraz", "rower"), List("ta", "owe", "Ala ")))
    println(find(List("test1", "test2", "test3", "test4"), Nil))
    println(find(List("test1", "test2", "test3", "test4"), List("4", "")))

    print("\n\n")

    println("Testy do zadania 2:")
    println("joinLists(List(5,4,3,2), List(1,0), List(9)) = " + joinLists(List(5,4,3,2), List(1,0), List(9)))
    println("joinLists(Nil, Nil, Nil) = " + joinLists(Nil, Nil, Nil))
    println("joinLists(List(42.1, 2.56, 1.0), Nil, List(3.33, 4.4)) = " + joinLists(List(42.1, 2.56, 1.0), Nil, List(3.33, 4.4)))

    print("\n\n")

    println("Testy do zadania 2 (wersja bez rekurencji ogonowej:")
    println("joinListsNoTial(List(5,4,3,2), List(1,0), List(9)) = " + joinListsNoTail(List(5,4,3,2), List(1,0), List(9)))
    println("joinListsNoTail(Nil, Nil, Nil) = " + joinListsNoTail(Nil, Nil, Nil))
    println("joinListsNoTail(List(42.1, 2.56, 1.0), Nil, List(3.33, 4.4)) = " + joinListsNoTail(List(42.1, 2.56, 1.0), Nil, List(3.33, 4.4)))

    print("\n\n")
  }

  def find (baza:List[String], zap:List[String]): List[String] =
    def compare (s1:String, spom:String): Boolean =
      @tailrec
      def comp (s1:String, s2:String, syt:Boolean): Boolean =
        syt match
          case false =>
            if s1 == "" then false
            else if s1.head == s2.head then comp(s1.tail, s2.tail, true)
            else comp(s1.tail, s2, false)
          case true =>
            if s2 == "" then true
            else if s1 == "" then false
            else if s1.head == s2.head then comp(s1.tail, s2.tail, true)
            else comp(s1, spom, false)
      if spom == "" then false else comp(s1, spom, false)
      // złożoność: obliczeniowa - kwadratowa = K*L, gdzie K i L to długości dwóch list
      // pamięciowa - stała ze względu na rekurencję ogonową
    @tailrec
    def checkOne (baza:String, zap:List[String]): Boolean =
      if zap == Nil then false
      else if compare(baza, zap.head) then true
      else checkOne(baza, zap.tail)
      // złożoność: obliczeniowa - K*L*N, gdzie N to liczba zapytań a K*L to złożoność compare
      // pamięciowa - stała, ponieważ mamy rekurencję ogonową oraz compare ma złożoność pamięciową stałą
    @tailrec
    def checkAll (baza:List[String], zap:List[String], wyn:List[String]): List[String] =
      if baza == Nil then wyn
      else if checkOne(baza.head, zap) then checkAll(baza.tail, zap, baza.head::wyn)
      else checkAll(baza.tail, zap, wyn)
    checkAll(reverse(baza), zap, Nil)
    // złożoność: obliczeniowa - K*L*N*M+M, gdzie M to liczba elementów w bazie danych, a K*L*N to złożoność checkOne (+M wynika z wywołania reverse)
    // pamięciowa - stała, dzięki rekurencji ogonowej oraz złożonościach pamięciowych checkOne i reverse będących także stałycch


  def reverse [A](lista: List[A]): List[A] =
    @tailrec
    def rev (lista:List[A], wynik:List[A]): List[A] =
      if lista == Nil then wynik else rev (lista.tail, lista.head::wynik)
    rev(lista, Nil)
    // złożoność: obliczeniowa - liniowa = N, gdzie N to długość listy; pamięciowa - stała dzięki rekurencji ogonowej


  def joinLists [A](lista1:List[A], lista2:List[A], lista3:List[A]): List[A] =
    @tailrec
    def join (listL:List[A], listP:List[A]): List[A] =
      listL match
        case h::t => join(t, h::listP)
        case Nil => listP
    join(reverse(lista1), join(reverse(lista2), lista3))
    // złożoność: obliczeniowa - liniowa = 2*(N+M), gdzie N to długość listy1 a M to długość listy2;
    // pamięciowa - stała dzięki rekurencji ogonowej


  def joinListsNoTail [A](lista1:List[A], lista2:List[A], lista3:List[A]): List[A] =
    def join (listL:List[A], listP:List[A]): List[A] =
      listL match
        case h::t => h :: join(t, listP)
        case Nil => listP
    join(lista1, join(lista2, lista3))
    // złożoność: obliczeniowa - liniowa = N+M, gdzie N to długość listy1 a M to długość listy2;
    // pamięciowa - liniowa około N+M, poniważ join(2, 3) wywoła się M razy, join(1, 2+3) wywoła się N razy, a każde z wywołań zajmie pamięć

}
