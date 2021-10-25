object ListLab3 {

  def main(args: Array[String]): Unit = {
   println(length(List('d')))
   println(combine(List('a','b','c','d','e'), List('1','2','3')))

   println(splitList(List(-3,-6, 7,-9,13)))
   println(splitList(Nil))
   println(splitList(List(0,1,2,3,-10,4,6)))
   println(splitList(List(0, 2, 4, 6)))
   println(splitList(List(-3, -1, -5)))
   println(splitList(List(0,1,2,2,3)))

    println(length(List(5,4,3,2)))
    println(length(Nil))
    println(length(List('1','5','7')))
    println(length(List("To ja")))

    println(combine(List(5,4,3,2), List(1, 2, 3, 4, 5, 6)))
    println(combine(Nil, Nil))
    println(combine(Nil, List(1, 2, 3, 4, 5, 6)))
    println(combine(List(5,4,3,2), Nil))


  }

  def splitList(xs: List[Int]): (List[Int], List[Int]) =
      (xs) match
        case (Nil) =>(List(), List())
        case (h::t) =>
          val x = splitList(t)
          if h < 0 then (h::x._1,x._2)
          else if h > 0 && h%2 == 1 then  (x._1,h::x._2)
          else  (x._1,x._2)



def length[A](as: List[A]): Int = {
  def lengthRec[A](as: List[A], k: Int): Int =
    (as) match
      case (Nil) => k
      case (h :: t) => lengthRec(t,k+1)
  lengthRec(as,0)}


def combine[A](xs: List[A], ys: List[A]): List[A] =
  (xs,ys) match
    case (h::t,h2::t2) => h::combine(ys,t)
    case (h::t,_) => xs
    case (_,h::t) => ys
    case (_,_) => List()
}