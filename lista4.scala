import scala.annotation.tailrec

/*
hasSegmentAt

funkcja hasSegmentAt mazłożoność obliczeniową O(n) gdzie n to długość szukanego segmentu,
ponieważ metoda startsWithRec wykona się w najgorszym przypadku tyle razy ile segment ma znaków
hasSegmentAt ma złożoność pamięciową O(1) ponieważ zawsze będzie definiować zmienne o takiej samej wielkości

jej wersja bez rekurencji ogonowej będzie miała obie złożoności O(n) ponieważ będzie potrzebowała pamięci na stosie
dla wywołań funkcji startsWithRec

wersję bez rekurencji ogonowej można napisać po prostu zamieniając kolejność operandów w wyrażeniu logicznym w "else" w startsWithRec,
więc jej nie pisałem, tak samo jest dla isSubstring

isSubstring

n - długość stringa którego przeszukujemy
k - długość szukanego substringa

Ilość wywołań funkcji isSubstringRec zależy liniowo od n.
Dla każdego wywołania wykonujemy funkcję hasSegmentAt o złożoności liniowej O(k).
Łącznie mamy złożoność obliczeniową O(n * k).

Złożoność pamięciowa wersji z rekurencją ogonową to O(1).
Bez rekurencji ogonowej w najgorszym przypadku stos będzie się składał maksymalnie z n - k ramek isSubstringRec oraz k ramek hasSegmentAt,
więc złożoność pamięciowa to O(n - k + k) = O(n)

filter

Metoda filter wykona się raz dla każdego elementu z listy więc ma złożoność obliczeniową O(n) gdzie n to długość listy wejściowej.
Metoda filter potrzebuje pamięci na ramki wywołań rekurencyjnych i zapisanie wyniku, oba zależą liniowo od n,
więc złożoność pamięciowa to O(n)

Wersja z rekurencją ogonową dodatkowo musi odwrócić wynik więc ma złożoność O(2n) czyli O(n).
Ma złożoność pamięciową O(n), ponieważ musi zapisać odwrócony wynik i wynik.

find
n - długość listy przeszukiwanych stringów
N - długość listy szukanych substringów
k - średnia długość przeszukiwanych stringów
l - średnia długość szukanych substringów

Funkcja find wywołuje metodę filter która wywoła predykat n razy.
Każde (nierekurencyjne) wywołanie predykatu doprowadzi do wywołania isSubstring N razy.
W tym kontekście isSubstring ma złożoność O(k * l)
Razem otrzymujemy złożoność O(n * N * k * l)

Funkcja find ma złożoność pamięciową metody filter plus potrzebuje miejsca na maksymalnie N ramek dla predykatu,
więc złożoność pamięciowa funkcji find to O(n + N) dla wersji bez rekurencji ogonowej i O(n) dla ogonowej.

join3Lists ma złożoność obliczeniową O(n) gdzie n to suma długości listy pierwszej i drugiej,
ponieważ tyle elementów musi dodać do listy trzeciej.
wersja z rekurencją ogonową ma taką samą złożoność obliczeniową,
ponieważ odwrócenie pierwszych dwóch list wykonuje się w czasie O(n) ale tylko raz.

Złożoność pamięciowa to O(n) gdzie n to łączna długość wszystkich list,
ponieważ potrzebne jest miejsce na wynik.
Odwrócenie listy również wymaga pamięci ale tylko raz i jest ona zwalniana zanim funkcja zacznie łączyć listy.

*/

object lista4 {

  val hasSegmentAt = (segment: String) => (startIndex: Int) => (string: String) =>
    val segmentLength = segment.length

    if segmentLength > string.length - startIndex then false
    else
      @tailrec
      def startsWithRec(stringIndex: Int, segmentIndex: Int): Boolean =
        if segmentIndex >= segmentLength then true
        else segment.charAt(segmentIndex) == string.charAt(stringIndex) && startsWithRec(stringIndex + 1, segmentIndex + 1)
        //by pozbyć się rekurencji ogonowej należy zamienić kolejność operandów linijkę wyżej
      startsWithRec(startIndex, 0)


  val isSubstring = (substring: String, string: String) =>
    val iterationEndIndex = string.length - substring.length

    if iterationEndIndex < 0 then false
    else
      val isSubstringAt = hasSegmentAt(substring)

      @tailrec
      def isSubstringRec(index: Int): Boolean =
        if index > iterationEndIndex then false
        else isSubstringAt(index)(string) || isSubstringRec(index + 1)
        //by pozbyć się rekurencji ogonowej należy zamienić kolejność operandów linijkę wyżej

      isSubstringRec(0)

  def filter[A](predicate: A => Boolean, list: List[A]): List[A] =
    if list == Nil then Nil
    else if predicate(list.head) then list.head :: filter(predicate, list.tail)
    else filter(predicate, list.tail)

  def filterTailRec[A](predicate: A => Boolean, list: List[A]): List[A] =
    def filterRec(acc: List[A], list: List[A]): List[A] =
      if list == Nil then reverseList(acc)
      else if predicate(list.head) then filterRec(list.head :: acc, list.tail)
      else filterRec(acc, list.tail)

    filterRec(Nil, list)

  val find = (list: List[String], searchedPhrases: List[String]) =>
    def includesSomePhrasesPredicate(phrases: List[String])(str: String): Boolean =
      if phrases == Nil then false
      else includesSomePhrasesPredicate(phrases.tail)(str) || isSubstring(phrases.head, str)

    filter(includesSomePhrasesPredicate(searchedPhrases), list)

  val findTail = (list: List[String], searchedPhrases: List[String]) =>
    @tailrec
    def includesSomePhrasesPredicate(phrases: List[String])(str: String): Boolean =
      if phrases == Nil then false
      else isSubstring(phrases.head, str) || includesSomePhrasesPredicate(phrases.tail)(str)

    filterTailRec(includesSomePhrasesPredicate(searchedPhrases), list)
  //zadanie 2

  def reverseList[A](xs: List[A]): List[A] =
    @tailrec
    def reverseRec(acc: List[A],xs: List[A]): List[A] =
      if xs == Nil then acc
      else reverseRec(xs.head :: acc, xs.tail)

    reverseRec(Nil, xs)

  def join3Lists[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
    (firstList, secondList) match
      case (Nil, Nil) => thirdList
      case (Nil, hd2 :: tl2) => hd2 :: join3Lists(Nil, tl2, thirdList)
      case (hd1 :: tl1, secondList) => hd1 :: join3Lists(tl1, secondList, thirdList)


  def join3ListsTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
    @tailrec
    def join3ListsRec(firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] =
      (firstList, secondList) match
        case (Nil, Nil) => thirdList
        case (hd1 :: tl1, Nil) => join3ListsRec(tl1, Nil, hd1 :: thirdList)
        case (firstList, hd2 :: tl2) => join3ListsRec(firstList, tl2, hd2 :: thirdList)

    join3ListsRec(reverseList(firstList), reverseList(secondList), thirdList)


  def main(args: Array[String]): Unit =
//    val startsWithKot = hasSegmentAt("kot")(0)
//    println(startsWithKot("ala ma kota"))
//    println(startsWithKot("ola ma ose"))
//    println(startsWithKot("kot ma ale"))
//    println(startsWithKot("kot"))
//    println(startsWithKot(""))
//    println(hasSegmentAt("")(0)(""))
//    println(hasSegmentAt("")(0)("rgefgsdfg"))
//
//    println("------hasSegmentAt------")
//
//    println(hasSegmentAt("kot")(1)("kkot"))
//    println(hasSegmentAt("kot")(0)("kkot"))
//    println(hasSegmentAt("index0168")(1)("iindex0168210"))
//    println(hasSegmentAt("ale")(9)("a kot ma ale"))
//
//    println("-----isSubstring-------")
//
//    println(isSubstring("ala", "ala nadal ma kota"))
//    println(isSubstring("osa", "a kot ma ale"))
//    println(isSubstring("osa", "natomiast osa nie ma nic"))

    println("-----find-------")

    println(filter((x: Int) => x % 2 == 0, List(1,2,3,4,5,6,7,8,9,10)))

    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")))
    println(find(List("rtgrsgh", "xyzabc", "abcxyz", "xyz123", "123xyz", "grsg", "xy1z23", "aggnigabciwbnf", "abc", "xyz", ""), List("abc", "xyz")))
    println(find(List(), List("dziure w calym")))
    println(find(List("ma", "wypisac", "wszystko", "jak", "leci"), List("")))

    println("------findTail-----")

    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")))
    println(findTail(List("rtgrsgh", "xyzabc", "abcxyz", "xyz123", "123xyz", "grsg", "xy1z23", "aggnigabciwbnf", "abc", "xyz", ""), List("abc", "xyz")))
    println(findTail(List(), List("dziure w calym")))
    println(findTail(List("ma", "wypisac", "wszystko", "jak", "leci"), List("")))

    println("------join3Lists------")

    println(join3Lists(List(1,2,3), List(4,5,6), List(7,8,9)))
    println(join3Lists(Nil, Nil, Nil))
    println(join3Lists(List(1,2,3), Nil, List(7,8,9)))

    println("-----join3ListsTail-------")

    println(join3ListsTail(List(1,2,3), List(4,5,6), List(7,8,9)))
    println(join3ListsTail(Nil, Nil, Nil))
    println(join3ListsTail(List(1,2,3), Nil, List(7,8,9)))
}
