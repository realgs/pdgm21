object Lista4 {

  import scala.annotation.tailrec

  //Zad 1.

  val emptyString = ""
  val space = ' '

  @tailrec
  def checker(word: String, wanted: String, leftToCheck: String, flag: Boolean): Boolean = //sprawdza czy string wanted zawiera się w string word
    word match
      case _ if leftToCheck == emptyString => true
      case _ if (word == emptyString || word.head != space) && leftToCheck.head == space => checker(word.tail, wanted, leftToCheck.tail, false)
      case "" => false
      case _ if word.head == leftToCheck.head => checker(word.tail, wanted, leftToCheck.tail, false)
      case _ if flag => checker(word.tail, wanted, wanted, false)
      case _ => checker(word, wanted, wanted, true)

  // n- długoś word, m- długość wanted
  // złożoność pamięciowa: O(1)
  // złożoność czasowa: W(n)


  def removeSpace(list: List[String]): List[String] = //usuwa spacje na początku słów
    @tailrec
    def removeSpaceHelper(list: List[String], string: String, acc: List[String]): List[String] =
      if string != emptyString && string.head == space then removeSpaceHelper(list, string.tail, acc)
      else
        list match
          case _ :: listSecHead :: _ => removeSpaceHelper(list.tail, listSecHead, string :: acc)
          case listHead :: _ => removeSpaceHelper(list.tail, listHead, string :: acc)
          case Nil => acc

    removeSpaceHelper(list, list.head, Nil)

  // k- długość listy "list"    m- długość string
  // złożoność pamięciowa: W(k)
  // złożoność czasowa: W(m*k)

//////////
  def find2(xs: List[String], wanted: String): List[String] =
    def findHelper(xs: List[String], wantedList: List[Char], acc: List[String]): List[String] =
      def checker(word: List[Char], leftToCheck: List[Char]): Boolean =
        (word, leftToCheck) match
          case (wordh :: wordt, lefth :: leftt) => if wordh == lefth then checker(wordt, leftt) else checker(wordt, wantedList)
          case (_, Nil) => true
          case (_, _) => false

      if wanted == emptyString then Nil
      else
        xs match
          case xh :: xt => if checker(xh.toList, wantedList) then findHelper(xt, wantedList, xh :: acc) else findHelper(xt, wantedList, acc)
          case Nil => acc

    val want = removeSpace(List(wanted)).head
    findHelper(xs, want.toList, Nil)

  // l- długość listy xs      m- długość wantedList    n- długość word
  // złożoność pamięciowa: (n^2/2 + m^2/2 + m + 1) + (2l) + l^2/2
  // złożoność czasowa: W(m + (m + l*n) + n*l)

/////////

  def find(xs: List[String], wanted: String): List[String] =
    def findHelper(xs: List[String], wanted: String): List[String] =
      if wanted == emptyString then Nil
      else
        xs match
          case xh :: xt => if checker(xh, wanted, wanted, false) then xh :: findHelper(xt, wanted) else findHelper(xt, wanted)
          case Nil => Nil

    findHelper(xs, removeSpace(List(wanted)).head)

  // l- długość listy xs      m- długość wanted    n- długość string w liście xs
  // złożoność pamięciowa: k + l^2/2
  // złożoność czasowa: W(m + n*l)


  def findTail(xs: List[String], wanted: String): List[String] =
    @tailrec
    def findHelper(xs: List[String], wanted: String, acc: List[String]): List[String] =
      if wanted == emptyString then Nil
      else
        xs match
          case xh :: xt => if checker(xh, wanted, wanted, false) then findHelper(xt, wanted, xh :: acc) else findHelper(xt, wanted, acc)
          case Nil => acc

    findHelper(xs, removeSpace(List(wanted)).head, Nil)

  // l- długość listy xs      m- długość wanted    n- długość string w liście xs
  // złożoność pamięciowa: W(k + k + l + l)
  // złożoność czasowa: W(m + n*l)


  def findN(xs: List[String], wantedList: List[String]): List[String] =
    def findHelper(xs: List[String], wantedList: List[String], leftToCheck: List[String]): List[String] =
      (xs, leftToCheck) match
        case (xh :: xt, leftToCheckHead :: leftToCheckTail) => if leftToCheckHead != emptyString then if checker(xh, leftToCheckHead, leftToCheckHead, false) then xh :: findHelper(xt, wantedList, wantedList) else findHelper(xs, wantedList, leftToCheckTail)
                                                               else return Nil
        case (xh :: xt, Nil) => findHelper(xt, wantedList, wantedList)
        case (Nil, _) => Nil

    val wanted = removeSpace(wantedList)
    findHelper(xs, wanted, wanted)

  // l- długość listy xs    k- długość listy wantedList     m- długość wanted     n- długość string w liście xs
  // złożoność pamięciowa: W(k + k + l*(l+2k))
  // złożoność czasowa: W(m*k + n*l*k)


  def findNTail(xs: List[String], wantedList: List[String]): List[String] =
    @tailrec
    def findHelper(xs: List[String], wantedList: List[String], leftToCheck: List[String], acc: List[String]): List[String] =
      (xs, leftToCheck) match
        case (_, leftToCheckHead :: leftToCheckTail) if leftToCheckHead == emptyString => Nil
        case (xh :: xt, leftToCheckHead :: leftToCheckTail) => if checker(xh, leftToCheckHead, leftToCheckHead, false) then findHelper(xt, wantedList, wantedList, xh :: acc) else findHelper(xs, wantedList, leftToCheckTail, acc)
        case (xh :: xt, Nil) => findHelper(xt, wantedList, wantedList, acc)
        case (Nil, _) => acc

    val wanted = removeSpace(wantedList)
    findHelper(xs, wanted, wanted, Nil)

  // l- długość listy xs    k- długość listy wantedList     m- długość wanted     n- długość string w liście xs
  // złożoność pamięciowa: W(k + k + l+k + l)
  // złożoność czasowa: W(m*k + n*l*k)


  //Zad 2.
  def joinLists[A](xs: List[A], ys: List[A], zs: List[A]): List[A] =
    (xs, ys, zs) match
      case (xh :: xt, _, _) => xh :: joinLists(xt, ys, zs)
      case (_, yh :: yt, _) => yh :: joinLists(xs, yt, zs)
      case (_, _, zh :: zt) => zh :: joinLists(xs, ys, zt)
      case (_) => Nil
  //złożoność pamięciowa: n^2/2 + m^2/2 + l^2/2
  //złożoność czasowa: n+m+l


  def reverse[A](xs: List[A]): List[A] =
    xs match
      case xh :: xt => reverse(xt) ::: List(xh)
      case Nil => Nil

  def joinListsTailrec[A](xs: List[A], ys: List[A], zs: List[A]): List[A] =
    @tailrec
    def joinListsHelper[A](xs: List[A], ys: List[A], zs: List[A], acc: List[A]): List[A] =
      (xs, ys, zs) match
        case (xh :: xt, _, _) => joinListsHelper(xt, ys, zs, xh :: acc)
        case (_, yh :: yt, _) => joinListsHelper(xs, yt, zs, yh :: acc)
        case (_, _, zh :: zt) => joinListsHelper(xs, ys, zt, zh :: acc)
        case (_) => reverse(acc)

    joinListsHelper(xs, ys, zs, Nil)
  //złożoność pamięciowa: W(n + m + l + (n + m + l)^2/2)
  //złożoność czasowa: 2(n+m+l)


  def main(args: Array[String]): Unit = {
    println(joinLists(List(1, 2, 3, 4), List(5, 6, 7), List(8, 9, 10)))
    println(joinListsTailrec(List(1, 2, 3, 4), List(5, 6, 7), List(8, 9, 10)))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "  index0168"))
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), " index0168"))
    println(findNTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "  index0169")))
    println(findN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "  index0169")))

    println(find(List("bbaadsf", "dabbda", "wabba", "abba"), " "))
    println(findTail(List("bbaadsf", "dabbda", "wabba", "abba"), " "))
    println(findNTail(List("bbaadsf", "dabbda", "wabba", "abba"), List(" ")))
    println(findN(List("bbaadsf", "dabbda", "wabba", "abba"), List(" ")))

    println(find(List("Ala ma kota", "dabbda", "wabba", "abba"), "   ma kota    "))
  }

}

