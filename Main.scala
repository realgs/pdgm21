object Main {
  import scala.annotation.tailrec

  // Zad 1
  // Pierwsza funkcja
  // Zlozonosc obliczeniowa - n^4
  // Uzasadnienie - Zalozmy najgorszy przypadek. Musimy przeiterowac przez cala liste i przez wszystkie
  // elementy. Juz to samo da nam kwadratowa zlozonosc. Dodatkowo celem weryfikacji czy dany element
  // znajduje sie w elemencie listy musimy przeiterowac przez znaki w elementach listy. W najgorszym
  // przypadku iterujemy wszystkie znaki aż do końca ciagu (np. "aaaa", a szukany element to "aaaaa")
  // co zwieksza nam zlozonosc do n^4
  //
  // Zlozonosc pamieciowa - liniowa
  // Uzasadnienie - nie tworzymy zadnej kopii listy po drodze, wiec jedyne co zuzywa pamiec
  // to lista poczatkowa, lista elementow i lista koncowa (wynik)
  //
  // Druga funkcja
  // Zlozonosc obliczeniowa - n^4
  // Uzasadnienie - jedyna roznica jest to, ze musimy odwrocic liste. Operacja odwrocenia listy
  // ma zlozonosc liniowa i wykonujemy ja tylko raz, wiec nie zmieni nam ona ostatecznej zlozonosci obliczeniowej
  // Zlozonosc pamieciowa - liniowa
  // Uzasadnienie - pamiec zajma nam lista wejsciowa, lista elementow, lista po wywolaniu funkcji
  // i odwrocona lista (rezultat koncowy). Kazda z tych list bedzie miala zlozonosc liniowa
  // wiec jak zsumujemy te zlozonosci to nadal bedziemy mieli zlozonosc liniowa.
  //
  // Porownanie funkcji
  // Pamietajmy, ze rekursja ogonowa jest optymalizowana przez kompilator i dzieki temu
  // mamy mniej wywolan funkcji na stosie. Jednak nalezy miec na uwadze, ze rekursja ogonowa
  // w tym przypadku jest lepsza od rekursji nieogonowej tylko wtedy kiedy mamy do czynienia
  // z bardzo duzymy listami i wywolujemy wiele razy rekursywnie funkcje. W innym przypadku
  // wiecej pamieci i czasu zuzyjemy na obliczenia niz jest to potrzebne

  def findNoTail(list: List[String], elements: List[String]): List[String] = {
    def doesContainOneOfElements(value: String, elements: List[String]): Boolean = {
      if elements == Nil then false
      else isElementPartOf(value, elements.head) || doesContainOneOfElements(value, elements.tail)
    }

    def isElementPartOf(value: String, element: String): Boolean = {
      (value, element) match {
        case (_, "") => true
        case ("", _) => false
        case (value, element) => doesContainElement(value, element) || isElementPartOf(value.tail, element)
      }
    }

    def doesContainElement(value: String, element: String): Boolean = {
      (value, element) match {
        case (_, "") => true
        case ("", _) => false
        case (value, element) => value.head == element.head && doesContainElement(value.tail, element.tail)
      }
    }

    // Helper function needed so that we can accomodate for special situations
    def iterateList(list: List[String], elements: List[String]): List[String] = {
      if list == Nil then Nil
      else if doesContainOneOfElements(list.head, elements) then list.head :: iterateList(list.tail, elements)
      else iterateList(list.tail, elements)
    }

    if elements == Nil then list else iterateList(list, elements)
  }

  def findTail(list: List[String], elements: List[String]): List[String] = {
    @tailrec
    def doesContainOneOfElements(value: String, elements: List[String]): Boolean = {
      if elements == Nil then false
      else isElementPartOf(value, elements.head) || doesContainOneOfElements(value, elements.tail)
    }

    @tailrec
    def isElementPartOf(value: String, element: String): Boolean = {
      (value, element) match {
        case (_, "") => true
        case ("", _) => false
        case (value, element) => doesContainElement(value, element) || isElementPartOf(value.tail, element)
      }
    }

    @tailrec
    def doesContainElement(value: String, element: String): Boolean = {
      (value, element) match {
        case (_, "") => true
        case ("", _) => false
        case (value, element) => value.head == element.head && doesContainElement(value.tail, element.tail)
      }
    }

    // Helper function needed so that we can accomodate for special situations
    @tailrec
    def iterateList(list: List[String], elements: List[String], result: List[String]): List[String] = {
      if list == Nil then result
      else if doesContainOneOfElements(list.head, elements) then iterateList(list.tail, elements, list.head :: result)
      else iterateList(list.tail, elements, result)
    }

    @tailrec
    def reverseList(list: List[String], outputList: List[String]): List[String] = {
      list match {
        case h :: s => reverseList(s, h::outputList)
        case Nil => outputList
      }
    }

    if elements == Nil then list else reverseList(iterateList(list, elements, Nil), Nil)
  }

  // Zad 2
  // Pierwsza funkcja
  // Zlozonosc obliczeniowa - liniowa wzgledem dlugosci pierwszej i drugiej listy
  // Uzasadnienie - Do stworzenia list uzywamy :: (prepend) o zlozonosc O(1)
  // Musimy przeiterowac przez cale firstList i secondList zanim bedziemy mogli
  // utworzyc koncowa liste
  // Zlozonosc pamieciowa - liniowa wzgledem dlugosci wszystkich list
  // Uzasadnienie - nie tworzymy zadnej kopii listy po drodze, wiec jedyne co zuzywa pamiec
  // to listy poczatkowe i lista koncowa skladajaca sie z list poczatkowych
  //
  // Druga funkcja
  // Zlozonosc obliczeniowa - liniowa wzgledem dlugosci pierwszej, drugiej i trzeciej listy
  // Uzasadnienie - w tej funkcji musimy przeiterowac przez wszystkie trzy tablice. Dodatkowo
  // musimy przeiterowac jeszcze raz przez cala liste, zeby moc ja odwrocic. Obie operacje
  // maja zlozonosc liniowa, wiec jak je zsumujemy to otrzymamy zlozonosc liniowa
  // Zlozonosc pamieciowa - liniowa wzgledem dlugosci wszystkich list
  // Uzasadnienie - pamiec zajma nam listy wejsciowe, lista po wywolaniu funkcji i odwrocona lista (rezultat koncowy).
  // Kazda z tych list bedzie miala zlozonosc liniowa  wiec jak zsumujemy te zlozonosci to nadal bedziemy mieli
  // zlozonosc liniowa.
  //
  // Porownanie funkcji
  // Mogloby sie wydawac, ze druga funkcja jest w kazdym wzgledzie gorsza od pierwszej
  // Ale nie zapominajmy, ze dzieki rekursji ogonowej kompilator zoptymalizuje rekursywne wywolania
  // w drugiej funkcji dzieki czemu glebokosc stosu wywolan funkcji bedzie stala, a nie wzrastala
  // jak w pierwszym przypadku

  // This is not tail recursion because function doesn't only call itself
  def joinListsNoTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    (firstList, secondList, thirdList) match {
      case (Nil, Nil, tL) => tL
      case (Nil, h::s, tL) => h :: joinListsNoTail(Nil, s, tL)
      case (h::s, sL, tL) => h :: joinListsNoTail(s, sL, tL)
    }
  }

  def joinListsTail[A](firstList: List[A], secondList: List[A], thirdList: List[A]): List[A] = {
    // This functions is tail recursive because we only call the function or return the value
    @tailrec
    def joinListsInside(firstList: List[A], secondList: List[A], thirdList: List[A], outputList: List[A]): List[A] = {
      (firstList, secondList, thirdList) match {
        case (Nil, Nil, Nil) => outputList
        case (Nil, Nil, h::s) => joinListsInside(Nil, Nil, s, h :: outputList)
        case (Nil, h::s, tL) => joinListsInside(Nil, s, tL, h :: outputList)
        case (h::s, sL, tL) => joinListsInside(s, sL, tL, h :: outputList)
      }
    }
    @tailrec
    def reverseList(list: List[A], outputList: List[A]): List[A] = {
      list match {
        case h :: s => reverseList(s, h::outputList)
        case Nil => outputList
      }
    }

    reverseList(joinListsInside(firstList, secondList, thirdList, Nil), Nil)
  }

  def main(args: Array[String]): Unit = {
    println(joinListsNoTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListsNoTail(Nil, Nil, Nil) == Nil)
    println(joinListsNoTail(List("A", "B"), Nil, List("C")) == List("A", "B", "C"))
    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListsTail(Nil, Nil, Nil) == Nil)
    println(joinListsTail(List("A", "B"), Nil, List("C")) == List("A", "B", "C"))

    println(findNoTail(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
      "index0169224"), List("index0168")) == List("iindex016802", "iindex0168211", "iindex0168210"))
    println(findNoTail(List("aba", "bab", "cac"), List()) == List("aba", "bab", "cac"))
    println(findNoTail(List("abaa", "baba", "cac"), List("aa", "ba")) == List("abaa", "baba"))
    println(findTail(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
      "index0169224"), List("index0168")) == List("iindex016802", "iindex0168211", "iindex0168210"))
    println(findTail(List("aba", "bab", "cac"), List()) == List("aba", "bab", "cac"))
    println(findTail(List("abaa", "baba", "cac"), List("aa", "ba")) == List("abaa", "baba"))
  }
}
