def find(list: List[String], frases: String): List[String] ={

}





def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  (first, second) match
    case (h1::t1, l2) => h1 :: joinLists(t1, l2, third)
    case (Nil, h2::t2) => h2 :: joinLists(Nil, t2, third)
    case (Nil, Nil) => third
}


val a = List(1,2,3,4,5,6)
val b = List(7,8,9)
val c = List(10,11,12)
joinLists(a,b,c)