//Szymon Bak, 260431
object Lista6 {
  //zadanie 1
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
    def eachNElementInner[A](list: LazyList[A], index: Int): LazyList[A] =
      if index < m && list != LazyList() then
        if index % n == 0 then list.head #:: eachNElementInner(list.tail, index + 1)
        else eachNElementInner(list.tail, index + 1)
      else LazyList()
    eachNElementInner(list, 0)

  //zadanie 2
  def calculate(operator: Char, e1: Int, e2: Int): Int =
    operator match
      case '+' => e1 + e2
      case '-' => e1 - e2
      case '*' => e1 * e2
      case '/' => if e2 == 0 then throw new Exception("Division by zero") else e1 / e2
      case _ => throw new Exception("Invalid operator")

  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operator: Char): LazyList[Int] =
    def lazyExecuteInner(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match
        case (LazyList(), _) => list2
        case (_, LazyList()) => list1
        case (h1 #:: t1, h2 #:: t2) => calculate(operator, h1, h2) #:: lazyExecuteInner(t1, t2)
    lazyExecuteInner(list1, list2)

  //zadanie 3
  def lazyRepeat[A](list: LazyList[A], numberOfRepeats: LazyList[Int]): LazyList[A] =
    def lazyRepeatInner[A](list: LazyList[A], numberOfRepeats: LazyList[Int], count: Int): LazyList[A] =
      (list, numberOfRepeats) match
        case (_, LazyList()) => list
        case (LazyList(), _) => list
        case (h1 #:: t1, h2 #:: t2) =>  if count < h2 then h1 #:: lazyRepeatInner(list, numberOfRepeats, count + 1)
                                        else lazyRepeatInner(t1, t2, 0)
    lazyRepeatInner(list, numberOfRepeats, 0)

  trait Debug {
    //zadanie 4
    def debugName(): String = {
      this.getClass().getSimpleName()
    }

    //zadanie 5
    def debugVars() = {
      def debugVar(fields: List[java.lang.reflect.Field]): List[List[Any]] = {
        fields match
          case Nil => Nil
          case field :: t =>
            field.setAccessible(true)
            List(field.getName(), field.getType(), field.get(this)) :: debugVar(t)
      }
      debugVar(this.getClass().getDeclaredFields().toList)
    }
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }
  class SomeClass(x: Int, y: Double, z: String) extends Debug {
    var a: Int = x
    var b: Double = y
    var c: String = z
  }

  def main(args: Array[String]): Unit = {
    //zadanie 1
    println("Zadanie 1:")
    println("[5;6;3;2;1], 2, 3 -> " + (eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3)).toList)
    println("[5;6;3;2;1], 2, 4 -> " + (eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4)).toList)

    //zadanie 2
    println("\nZadanie 2:")
    println("[1;2;3], [2;3;4;5], '+' -> " + lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList)
    println("[1;2;3], [2;3;4;5], '*' -> " + lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList)
//    println("[1;2;3], [2;3;0;5], '/' -> " + lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 0, 5), '/').toList) // Throws error, Division by zero
//    println("[1;2;3], [2;3;4;5], '=' -> " + lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '=').toList) // Throws error, Invalid operator

    //zadanie 3
    println("\nZadanie 3:")
    println("[1;2;3], [0;3;1;4] -> " + lazyRepeat(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList)
    println("[1;2;3], [] -> " + lazyRepeat(LazyList(1, 2, 3), LazyList()).toList)
    println("[], [1, 2, 3, 4] -> " + lazyRepeat(LazyList(), LazyList(1, 2, 3, 4)).toList)
    println("[1, 2, 3], [1, 2, 3] -> " + lazyRepeat(LazyList(1, 2, 3), LazyList(1, 2, 3)).toList)

    //zadanie 4
    val p: Point = new Point(3, 4)
    val s: SomeClass = new SomeClass(22, 1.4578, "someclass")
    println("\nZadanie 4:")
    println("Point(3, 4) -> " + p.debugName())
    println("SomeClass(22, 1.4578, \"someclass\") -> " + s.debugName())
    //zadanie 5
    println("\nZadanie 5:")
    println("Point(3, 4) -> " + p.debugVars())
    println("SomeClass(22, 1.4578, \"someclass\") -> " + s.debugVars())
  }
}
