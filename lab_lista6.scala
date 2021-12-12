object lab_lista6 {

  //zadanie 1
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
    {
      def eachNElementHelper[A](list: LazyList[A], cur_index: Int, end_index: Int): LazyList[A] =
        {
          (list, cur_index) match {
            case (h #:: t, 0) => h #:: eachNElementHelper(t, cur_index+1, end_index)
            case (h #:: t, _) => if cur_index == end_index then LazyList() else if cur_index % n == 0 then h #:: eachNElementHelper(t, cur_index+1, end_index) else eachNElementHelper(t, cur_index+1, end_index)
            case (LazyList(), _) => LazyList()
          }
        }
      if n > 0 && m > 0 then eachNElementHelper(list, 0, m)
      else LazyList()
    }

  //zadanie 2
  def operation(number1: Double, number2: Double, operator: Char): Double =
    {
      operator match {
        case '+' => number1 + number2
        case '-' => number1 - number2
        case '*' => number1 * number2
        case '/' => if number2 != 0 then number1/number2 else throw new Exception("Dividing by 0!")
        case '^' => Math.pow(number1, number2)
        case _ => throw new Exception("Unsupported operation!")
      }
    }

  def lazyExecute(list1: LazyList[Double], list2: LazyList[Double], operator: Char): LazyList[Double] =
    {
      (list1, list2) match {
        case (h1 #:: t1, h2 #:: t2) => operation(h1, h2, operator) #:: lazyExecute(t1, t2, operator)
        case (LazyList(), _) => list2
        case (_, LazyList()) => list1
      }
    }

  //zadanie 3
  def repeat[A](elementsList: List[A], repsList: List[Int]): List[A] =
    {
      def repeatHelper[A](elementsList: List[A], repsList: List[Int], result: List[A]): List[A] =
        {
          (elementsList, repsList) match {
            case (h1 :: t1, h2 :: t2) => if h2 > 0 then repeatHelper(elementsList, (h2-1) :: t2, h1 :: result) else repeatHelper(t1, t2, result)
            case (Nil, _) => result.reverse
            case (_, Nil) => result.reverse
          }
        }
      repeatHelper(elementsList, repsList, Nil)
    }

  trait Debug {

    //zadanie 4
    def debugName(): String =
      getClass.getSimpleName

    //zadanie 5
    def debugVars(): List[Any] =
      {
        def debugVarsHelper(list: List[java.lang.reflect.Field], result: List[Any]): List[Any] =
          {
            list match {
              case head :: tail =>  head.setAccessible(true)
                                    debugVarsHelper(tail, List(head.getName, head.getType, head.get(this)) :: result)
              case Nil => result.reverse
            }
          }
        debugVarsHelper(getClass.getDeclaredFields.toList, Nil)
      }
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Person(name: String, surname: String, age: Int) extends Debug {
    var i: Int = 0
    var n: String = name
    var s: String = surname
    var a: Int = age
  }



  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).force)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).force)
    println(eachNElement(LazyList.from(2), 2, 10).force)
    println(eachNElement(LazyList.from(-10), 6, 13).force)
    println(eachNElement(LazyList.from(0), 4, 1).force)
    println(eachNElement(LazyList.from(0), -2, 10).force)
    println(eachNElement(LazyList(), 2, 3).force)
    println()

    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').force)
    println(lazyExecute(LazyList(5, 6, 7, 8, 9, 10), LazyList(-5, -6, -7, -8, -9, -10), '+').force)
    println(lazyExecute(LazyList(-1, -0.5, 0, 0.5, 1), LazyList(-1), '-').force)
    println(lazyExecute(LazyList(2, 1, 3, 7), LazyList(1, 9, 7, 0), '*').force)
    println(lazyExecute(LazyList(4, 2, 1, 0, 0.5, 0.25), LazyList(2, 1, 0.5, 2, 0.25, 0.125, 0), '/').force)
    println(lazyExecute(LazyList(0, 1, 2, 3, 4), LazyList(2, 3, 4, -1), '^').force)
    println(lazyExecute(LazyList(), LazyList(0), '-').force)
    //println(lazyExecute(LazyList(1), LazyList(0.5), ':').force)
    println()

    println(repeat(List(1, 2, 3), List(0, 3, 1, 4)))
    println(repeat(List('a', 'b', 'c', 'd', 'e', 'f'), List(0, 1, 0, 2, -1, 3, 0)))
    println(repeat(List(0.5, 1, 1.5, 2), List(2, 1, 0)))
    println(repeat(List(true, false), List(2, 1, 190)))
    println(repeat(List("Ala", "ma", "kota"), List(1, 1, 1)))
    println(repeat(List(-5, -4, -3, -2, -1), List()))
    println(repeat(List(), List(1, 2, 3, 4)))
    println()

    var p: Point = new Point(3, 4)
    var p2: Point = new Point(0, 0)
    var m: Person = new Person("Wojciech", "Dominiak", 20)
    var m2: Person = new Person("Jan", "", 55)
    println(p.debugName())
    println(p.debugVars())
    println(m.debugName())
    println(m.debugVars())
  }
}
