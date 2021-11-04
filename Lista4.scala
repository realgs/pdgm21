// Aleksandra Wolska

object Lista4 {
  //Zadanie 1 dla pojedynczego keyworda
  /*
  gdzie m = dlugosc elementu bazy
  n = ilosc elementow w badzie
  k - dlugosc keyworda
  1) Mamy jeden element z bazy i jednego keyworda. Złożoność będzie m * k, ponieważ kursory kazdego z nich musze przejść po każdym
  2) Mamy całą bazę i jednego keyworda. Złożonośc będzie m* k * n, ponieważ dodatkowo powtarzamy to n razy (dla kazdego elementu w bazie)
  3) Złożoności stałych (5 if-else) przy każdej iteracji nie uwzględniam, (przy uzględnieniu m*k*n*5), ponieważ stałe się ignoruje.
  4)Ostatecznie złozonośc obliczeniowa wynosi O(n*m*k)

  Jako że mamy tutaj do czynienia z rekurencją ogonową, złożoność pamięciową mamy stałą
  */

  def find(base: List[String], keyword: String): List[String] =
    def findHelper(base: List[String], keyword: String, baseElemCursor: Int, keywordCursor: Int, result: List[String]): List[String] =
      if(keyword == "") {
        return result
      }
      if (base == Nil) {                                                                                    //nie ma więcej elementów w bazie
        return result
      } else {
        if (keywordCursor == keyword.length) {                                                              //kursor keyworda == dlugość slowa, czyli dopasowaliśmy wszyskie znaki
          findHelper(base.tail, keyword, 0, 0, base.head :: result)
        } else {
            if (base.head.length == baseElemCursor) {                                                       //kursor elementu z bazy minął ostatni element, czyli nie dopasowano
            findHelper(base.tail, keyword, 0, 0, result)
          } else {
            if (base.head.charAt(baseElemCursor) == keyword.charAt(keywordCursor)) {                        //litery obecnych kursorów się zgadzają
              findHelper(base, keyword, baseElemCursor + 1, keywordCursor + 1, result)
            } else {
              findHelper(base, keyword, baseElemCursor + 1, keywordCursor, result)
            }
          }
        }
      }

    findHelper(base, keyword, 0, 0, List())




  //Zadanie 2 dla listy keywordów. Funkcja FindNPhrases otrzymuje dwie listy - z bazą oraz keywordami. Wewnętrzna funcja rekurencyjnie wywołuje się,
  // przekazując kolejne głowy listy keywordów do kolejnej funkcji - findOuterHelper, bardzo podobnej do tej z poprzedniego zadania, która zwraca
  // listę wynikową dla pojedynczego keyworda, po czym jest ona konkatenowana w wyjściowej funkcji.

  /*
  Złozonośc obliczeniowa jw plus
  5. Analizujemy je dla każdego osobnego keyworda. Długosc tablicy keywordów = l.
  6.Ostatecznie zlożonośc obliczeniowa = O(n*m*k*l)

  Mimo, że mamy utworzone dwie nowe funkcje, złożonośc pamięciowa nadal będzie stało, bo korzystamy z rekurencji ogonowej
  */

  def findOuterHelper(base: List[String], keyword: String, baseElemIndex: Int, keywordIndex: Int, result: List[String]): List[String] =

    if(keyword == "")
      return result

    if (base == Nil) {
      return result
    } else {
      if (keywordIndex == keyword.length) {
        findOuterHelper(base.tail, keyword, 0, 0, base.head :: result)
      } else {
        if (base.head.length == baseElemIndex) { //element sie skonczy;l
          findOuterHelper(base.tail, keyword, 0, 0, result)
        } else {
          if (base.head.charAt(baseElemIndex) == keyword.charAt(keywordIndex)) {
            findOuterHelper(base, keyword, baseElemIndex + 1, keywordIndex + 1, result)
          } else {
            findOuterHelper(base, keyword, baseElemIndex + 1, keywordIndex, result)
          }
        }
      }
    }


  def findNPhrases(base: List[String], keywordList: List[String]): List[String] =
    def findNHelper(base: List[String], keywordList: List[String], baseElemIndex: Int, keywordIndex: Int, result: List[String]): List[String] =
      if (keywordList != Nil) {
        findNHelper(base, keywordList.tail, 0, 0, result:::findOuterHelper(base, keywordList.head, 0, 0, List()))
      } else {
        return result
      }
    findNHelper(base, keywordList, 0, 0, List())




  //Zadanie 2
  /*
  Złozonośc obliczeniowa będzie wynosić sumę długości wszytskich list + liniowa złożoność dodawania każdego elementu do listy.
  Dla n = długośc wszytskich list, mamy
  O(n^2)

  Złożoność pamięciowa jest stała dla głównej funkcji, ponieważ jest ogonowa, zaś pomocnocza funkcją będzie mieć złożoność równą głębokości rekurencji
  czyli długości "obecnej listy", która wynosi maksymalnie tyle co najdłuższa lista. Przy wychodzeniu z funkcji ramki zostają ściągane.
  dlugosc najdłuższej listy = n, więc
  O(n)
  */
  def addToList[A](list: List[A], elem: A): List[A] =
    if list != Nil
    then list.head :: addToList(list.tail, elem)
    else List(elem)


  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    def joinHelper(list1: List[A], list2: List[A], list3: List[A], listResult: List[A]): List[A] =
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => listResult
        case (Nil, Nil, _) => joinHelper(list1, list2, list3.tail, addToList(listResult, list3.head))
        case (Nil, _, _) => joinHelper(list1, list2.tail, list3, addToList(listResult, list2.head))
        case (_, _, _) => joinHelper(list1.tail, list2, list3, addToList(listResult, list1.head))
      }
    joinHelper(list1, list2, list3, List())


  def main(args: Array[String]): Unit = {

    println("\nZadanie 1")
    println(find(List("halo", "ha", "maha", "tama", "haha", "yamaha"), "ha"))
    println(find(List("halo", "maha", "tama", "haha", "yamaha"), ""))
    println(find(List(), "nothing"))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println("\nZadanie 1 - dla kilku slow kluczowych")
    println(findNPhrases(List("abc", "ab", "bcd", "deft", "abdf", "cedr", "eedrtr", "alat"), List("ab", "ef", "ed", "at")))
    println(findNPhrases(List("abc", "bcd", "deft", "abdf", "cedr", "eedrtr", "alat"), List("")))
    println(findNPhrases(List(), List("ab", "ef", "ed", "at")))

    println("\nZadanie 2")
    println(joinLists(List(1, 2, 3, 4, 5), List(6, 7, 8, 9), List(10, 11, 12, 13)))
    println(joinLists(List(1, 1, 1, 1, 1, 1, 1, 1), List(2, 2, 2, 2), List(3, 3, 3, 3)))
    println(joinLists(List("Ala"), List("ma", "kota"), List("oraz", "psa")))
    println(joinLists(List(), List(), List()))

  }


}