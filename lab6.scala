object main{


  def eachNElement[A](list: LazyList[A] , n: Int , m: Int): LazyList[A] = {
    def eachNElementIN(list: LazyList[A] , Actidx: Int , result: LazyList[A]): LazyList[A] = {
      if Actidx == m then result.reverse
      else if Actidx % n == 0 then eachNElementIN(list.tail , Actidx +1 , list.head #:: result)
      else eachNElementIN(list.tail , Actidx +1 , result)
    }
    if m < 0 then throw new Exception("nie ma ujemnych pozycji w liście")
    else if n < 0 then throw new Exception("nie można brać ujemych elementów")
    else eachNElementIN(list , 0 , LazyList());
  }

  def lExecute(list1: LazyList[Int] , list2: LazyList[Int] , operation : Char): LazyList[Int] = {
    def lExecuteIn(l1: LazyList[Int] , l2: LazyList[Int] , ActOperation: (Int , Int) => Int): LazyList[Int] ={
      if !l1.nonEmpty then l2
      else if !l2.nonEmpty then l1
      else ActOperation(l1.head , l2.head) #:: lExecuteIn(l1.tail , l2.tail , ActOperation)
    }
    operation match {
      case ('+') => lExecuteIn(list1 , list2 , (x , y) => x + y)
      case ('-') => lExecuteIn(list1 , list2 , (x , y) => x - y)
      case ('/') => lExecuteIn(list1 , list2 , (x , y) => x / y)
      case ('*') => lExecuteIn(list1 , list2 , (x , y) => x * y)
      case ('^') => lExecuteIn(list1 , list2 , (x , y) => x ^ y)
      case _ => throw new Exception("Brak takiej operacji matematycznej")
    }
  }

  def Duplicate[A](list: List[A], repets: List[Int]): List[A] = {
    def DuplicateIn(list: List[A], repets: List[Int] , res: List[A]): List[A] = {
      (list , repets) match {
        case (h :: t , rh :: rt) =>
          if rh > 0 then DuplicateIn(list , (rh-1) :: rt , h :: res)
          else DuplicateIn(t , rt , res)
        case (_ , Nil ) => res.reverse
        case (Nil , _ ) => res.reverse
      }
    }
    DuplicateIn(list , repets , List())
  }

  trait Debug{
    def debugName(): String = {
      return getClass.getName
    }

    def debugVars(): List[Any] = {
      getClass.getDeclaredFields().toList.map(field =>
        field.setAccessible(true)
        List(field.getName , field.getType , field.get(this)))

    }
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {
    var p : Point = new Point(3 ,5)
    println(p.debugName())
    println(p.debugVars())
    println(eachNElement(LazyList(5,6,3,2,1) , 2 ,4 ).force)
    println(lExecute(LazyList(1,2,3),LazyList(2,3,4,5),'+').force)
    println(Duplicate(List(1,4,6,2),List(0,0,3,1)))

  }
}
