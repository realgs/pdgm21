import scala.annotation.tailrec

object Main {
  //Zad 1
  // obliczeniowa l*e*(m+n) gdzie l - długość listy list, e - długość listy elem , (m+n) - złożoność czasowa cont
  // pamięciowa stała
  def findTail(list: List[String], elem: List[String]): List[String] = {
    @tailrec
    def findInTail(list: List[String] , elem: List[String] , res: List[String] , listhelp: List[String]): List[String] = {
      (list,elem) match {
        case (Nil , Nil) => res
        case (Nil , _) => res
        case (_ , Nil) => res
        case (lh :: lt , eh :: et) => if cont(lh , eh) then findInTail(lt , listhelp , lh :: res , listhelp)
        else if et != Nil then findInTail(list , et , res , listhelp)
        else findInTail(lt , listhelp , res , listhelp)
      }
    }
    reverse(findInTail(list , elem , List() , elem))
    }

  // obliczeniowa l*e*(m+n) gdzie l - długość listy list, e - długość listy elem , (m+n) - złożoność czasowa cont
  // pamięciowa (n^2 + n) /2
  def find(list: List[String], elem: List[String]): List[String] = {
    def findIn(list: List[String], elem: List[String], listhelp: List[String]): List[String] = {
      (list, elem) match {
        case (Nil, Nil) => List()
        case (Nil, _) => List()
        case (_ , Nil) => List()
        case (lh :: lt, eh :: et) => if cont(lh, eh) then lh :: findIn(lt, listhelp , listhelp)
        else if et != List() then findIn(list , et , listhelp)
        else findIn(lt , listhelp , listhelp)
      }
    }
    findIn(list,elem,elem)
  }

  // obliczeniowa m + n ,gdzie m,n - długość wprowadzanych słów
  // pamięciowa stała 1
  def cont(ch: String , elem: String): Boolean = {
    (ch,elem) match {
      case ("","") => true
      case (_,"") => true
      case ("",_) => false
      case (h , eh) => if h.head == eh.head then cont(h.tail ,eh.tail)
                     else cont(h.tail , elem)
    }
  }


  // Zad 2

    // obliczeniowa l1+l2+l3 ,gdzie l1 - ilośc elementów list1 itp...
    // pamięciowa (n^2 + n)/2
    def joinList[A](list1: List[A] , list2: List[A] , list3: List[A]): List[A] ={
      (list1 , list2 , list3) match{
        case (Nil , Nil , t) => t
        case (Nil , h::s , t) => h::joinList(Nil , s , t)
        case(h::s, y , x) => h::joinList(s, y , x)
      }
    }

    // obliczeniowa n, gdzie n - ilość elementów listy
    // pamięciowa n, - gdzie n - ilość elementów listy
    def reverse[A](list: List[A]): List[A] ={
      @tailrec
      def reverseIn[A](list: List[A] , res: List[A]): List[A] ={
        if list == Nil then res
        else reverseIn(list.tail , list.head :: res);
      }
      reverseIn(list , List())
    }


    //obliczeniowa l1+l2+l3 ,gdzie l1 - ilośc elementów list1 itp...
    // pamięciowa l1+l2+l3, gdzie l1 - ilośc elementów list1 itp...
    def joinListTail[A](list1: List[A] , list2: List[A] , list3: List[A]): List[A] ={
      @tailrec
      def joinListTailIn(list1: List[A] , list2: List[A] , list3: List[A], out: List[A]): List[A] = {
        (list1,list2,list3) match {
          case (Nil , Nil , t) => t:::out
          case (Nil , Nil , h::t) => joinListTailIn(Nil , Nil , t , h::out)
          case (Nil , h :: t , s) => joinListTailIn(Nil , t , s , h::out)
          case(h::t , y , x) => joinListTailIn(t , y , x , h::out)
        }
      }
      reverse(joinListTailIn(list1 , list2 , list3 , List()))
    }

    def main(args: Array[String]):Unit ={
      println(joinList(List(5, 4, 3, 2), List(1, 0), List(9)))
      println(joinListTail(List(5, 4, 3, 2), List(1, 0), List(9)))
      println(findTail(List("ananas","amoniak","legia","barszcz","orangutan") , List("bar","an")))
      println(findTail(List("index0169" , "iindex0168202" , "iindex0168211" , "iindex0168210" , "iindex0169222" , "index0169224") , List("index0168" , "index169")))
      println(find(List("index0169" , "iindex0168202" , "iindex0168211" , "iindex0168210" , "iindex0169222" , "index0169224") , List("index0168" , "index169")))
      println(find(List("ananas","amoniak","legia","barszcz","orangutan") , List("an","b")))
    }
  }
