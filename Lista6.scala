import scala.::

object Lista6 {

  //Zadanie 1
  def eachElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
   if(n < 0) || (m < 0) then throw new Exception("n or m index are wrong!")
    def eachElementIn(list: LazyList[A], nprim: Int, mprim: Int): LazyList[A] =
      (list, nprim, mprim) match
        case (LazyList(), _, _) => LazyList()
        case (head #:: tail, _, 0) => LazyList()
        case (head #:: tail, 1, _) => head #:: eachElementIn(tail, n, mprim - 1)
        case (head #:: tail, _, _) => eachElementIn(tail, nprim - 1, mprim - 1)

    eachElementIn(list, 1, m)

  //Zadanie 2
  def lazyExecute[A](list1: LazyList[Int], list2: LazyList[Int], operation: Char): LazyList[Int] =
    def lazyExecuteIn[A](list1: LazyList[Int], list2: LazyList[Int], operationprim: Char): LazyList[Int] =
      (list1, list2, operationprim) match
        case (LazyList(), LazyList(), _) => LazyList()
        case (head1 #:: tail1, LazyList(), _) => head1 #:: lazyExecuteIn(tail1, LazyList(), operationprim)
        case (LazyList(), head2 #:: tail2, _) => head2 #:: lazyExecuteIn(LazyList(), tail2, operationprim)
        case (head1 #:: tail1, head2 #:: tail2, '+') => (head1 + head2) #:: lazyExecuteIn(tail1, tail2, '+')
        case (head1 #:: tail1, head2 #:: tail2, '-') => (head1 - head2) #:: lazyExecuteIn(tail1, tail2, '-')
        case (head1 #:: tail1, head2 #:: tail2, '*') => (head1 * head2) #:: lazyExecuteIn(tail1, tail2, '*')
        case (head1 #:: tail1, head2 #:: tail2, '/') => (head1 / head2) #:: lazyExecuteIn(tail1, tail2, '/')

    lazyExecuteIn(list1, list2, operation)


  //Zadanie 3
  def repeat[A](counter: Int, element: A): List[A] =
    (counter) match
      case 0 => Nil
      case _ => element :: repeat((counter - 1), element)


  def duplicate[A](list1: List[A], list2: List[Int]): List[A] =
    def duplicateIn[A](list1: List[A], list2: List[Int]): List[A] =
      (list1, list2) match
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (head1 :: tail1, head2 :: tail2) => repeat(head2, head1) ++ duplicateIn(tail1, tail2)

    duplicateIn(list1, list2)


  //Zadanie 4
  trait Debug:
    def debugName(): String = this.getClass().getSimpleName()

  //Zadanie 5
    def debugVars(): List[Any] =
      this.getClass.getDeclaredFields.toList.map(field =>{
        field.setAccessible(true);
        List(field.getName(), field.getType(), field.get(this))})


  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }
   class Student(gradev: Int, namev: String) extends Debug{
     var grade: Int = gradev
     var name: String = namev
   }

  def main(args: Array[String]): Unit = {

    System.out.println("Zadanie 1 (eachElement):")
    System.out.println(eachElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList)
    System.out.println(eachElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList)
    System.out.println(eachElement(LazyList(), 2, 4).toList)

    System.out.println("\nZadanie 2 (lazyExecute):")
    System.out.println(lazyExecute(LazyList(1, 2, 3), LazyList(), '+').toList)
    System.out.println(lazyExecute(LazyList(), LazyList(2, 3, 4, 5), '-').toList)
    System.out.println(lazyExecute(LazyList(), LazyList(), '+').toList)
    System.out.println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList)
    System.out.println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList)
    System.out.println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList)
    System.out.println(lazyExecute(LazyList(6, 4, 1), LazyList(3, 2, 1, 5), '/').toList)

    System.out.println("\nZadanie 3 (duplicate):")
    System.out.println(duplicate(List(1, 2, 3), List(0, 3, 1, 4)))
    System.out.println(duplicate(List(), List(0, 3, 1, 4)))
    System.out.println(duplicate(List(1, 2, 3), List()))
    System.out.println(duplicate(List(1, 2, 3), List(0, 0, 0)))
    System.out.println(duplicate(List(0, 3, 1, 4), List(1, 2, 3)))


    var p: Point = new Point(3, 4)
    var s: Student = new Student(5, "Justyna")

    System.out.println("\nZadanie 4 (debugName):")
    System.out.println(p.debugName())
    System.out.println(s.debugName())

    System.out.println("\nZadanie 5 (debugVars):")
    System.out.println(p.debugVars())
    System.out.println(s.debugVars())
  }
}


