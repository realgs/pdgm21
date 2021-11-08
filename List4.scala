object List4 {

  val sep = ';'

  def append[A](l1: List[A], l2: List[A]): List[A] =
    def help(l1: List[A], l2: List[A], res: List[A]): List[A] =
      (l1,l2) match {
        case (Nil, Nil) => res.reverse
        case (Nil, h::t) =>help(Nil, t, h::res)
        case (h::t, l2) => help(t, l2, h::res)
      }
    help(l1, l2, List())

  def reverseList[A](list: List[A]): List[A] =
    def reverse[A](res: List[A], input: List[A]): List[A] =
      input match {
        case Nil => res
        case h::t => reverse(h::res, t)
      }
    reverse(List(), list);

  def include(what: String, inWhat: String): Boolean =
    val whatList = what.toList;
    def help(what: List[Char], inWhat: List[Char], window: List[Char]): Boolean =
      (what, inWhat, window) match{
        case (Nil, _, _) => true
        case (_, _, Nil) => false
        case (_, Nil, _) => false
        case (h::t, hi::ti, hw::tw) => if h == hw then help(t, hi::ti, tw)
                                       else help(whatList, ti, ti)
      }
    help(whatList, inWhat.toList, inWhat.toList)

  def contains(c: Char, s:String): Boolean =
    def help(list: List[Char]): Boolean =
      list match {
        case Nil => false
        case h::t => if h==c then true
                     else help(t)
      }
    help(s.toList)

  def splitBySign(exp: String, sign: Char): List[String] =
    def help(list: List[Char], res: String): List[String] =
      list match {
        case Nil => List(res)
        case h::t => if h == sep then res :: help(t,"")
                     else help(t, res.appended(h))
      }
    help(exp.toList, "")

  def findTail(list: List[String], elem: String): List[String] =
    val elemsSplited = splitBySign(elem, sep)
    def help(list: List[String], res: List[String], elemList: List[String]): List[String] =
      (list, elemList) match {
        case (Nil, _) => res.reverse
        case (h::t, Nil) => help(t, res, elemsSplited)
        case (h::t,he::te) => if include(he, h) then help(t, h::res, elemsSplited)
                              else help(h::t, res, te)
        }
    help(list, List(), elemsSplited)

  def find(list: List[String], elem: String): List[String] =
    val elemsSplited = splitBySign(elem, sep)
    def help(list: List[String], elemList: List[String]): List[String] =
      (list, elemList) match {
        case (Nil, _) => List()
        case (h::t, Nil) => help(t, elemsSplited)
        case (h::t,he::te) => if include(he, h) then h :: help(t, elemsSplited)
        else help(h::t, te)
      }
    help(list, elemsSplited)

  def join3ListsTail[A](start:List[A], middle: List[A], end: List[A]): List[A] =
    def conector(start:List[A], middle: List[A], end: List[A], tmp: List[A]): List[A] =
      (start, middle, end) match {
        case (Nil, Nil, Nil) => reverseList(tmp)
        case (h::t, _, _) => conector(t, middle, end, h::tmp)
        case (Nil, h::t, _) => conector(Nil, t, end, h::tmp)
        case (Nil, Nil, h::t) => conector(Nil, Nil, t, h::tmp)
      }
    conector(start, middle, end, List());

  def join3Lists[A](start:List[A], middle: List[A], end: List[A]): List[A] =
    (start, middle, end) match {
      case (Nil, Nil, Nil) => List()
      case (h::t, _, _) => h::join3Lists(t, middle, end)
      case (Nil, h::t, _) => h::join3Lists(Nil, t, end)
      case (Nil, Nil, h::t) => h::join3Lists(Nil, Nil, t)
    }

  def main(args: Array[String]) : Unit = {

    println(find(List("tak1","tak2","nie3","nie4","kto","kot","taknie","nietakt","a","b"),"tak;nie"));
    println(find(List("b","a","aa","ab","c","ac","cc","db","af","b"),"a;b"));

    println(findTail(List("tak1","tak2","nie3","nie4","kto","kot","taknie","nietakt","a","b"),"tak;nie"));
    println(findTail(List("b","a","aa","ab","c","ac","cc","db","af","b"),"a;b"));

    val l1 = List(1,2,3,4);
    val l2 = List(5,6);
    val l3 = List(7,8,9);

    println(join3ListsTail(l1, l2, l3));

    println(join3Lists(l1, l2, l3));
  }
}
