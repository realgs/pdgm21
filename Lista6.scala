import java.lang.System
import scala.annotation.tailrec

object Lista6 {


 //Zad 1
  def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def eachNElementHelper[A](lazyListHelper: LazyList[A], nHelper: Int, mHelper: Int): LazyList[A] = {
      (lazyListHelper, nHelper, mHelper) match
        case (LazyList(), _, _) => LazyList()
        case (head #:: tail, _, 0) => LazyList()
        case (head #:: tail, 1, _) => head #:: eachNElementHelper(tail, n, mHelper - 1)
        case (head #:: tail, _, _) => eachNElementHelper(tail, nHelper - 1, mHelper - 1)
    }

    if n<=0 || m <=0 then throw new Exception("Wrong value of m or n!")
    else eachNElementHelper(lazyList, 1, m)
  }

  //Zad 2
  def lazyExecute[A](lazyList1: LazyList[Int], lazyList2: LazyList[Int], operatorSign: Char): LazyList[Int] = {
    def lazyExecuteHelper[A](lazyList1Helper: LazyList[Int], lazyList2Helper: LazyList[Int], operatorSignHelper: Char): LazyList[Int] = {
      (lazyList1Helper, lazyList2Helper, operatorSignHelper) match
        case (head1 #:: tail1, head2 #:: tail2, '+') => (head1 + head2) #:: lazyExecuteHelper(tail1, tail2, '+')
        case (head1 #:: tail1, head2 #:: tail2, '-') => (head1 - head2) #:: lazyExecuteHelper(tail1, tail2, '-')
        case (head1 #:: tail1, head2 #:: tail2, '*') => (head1 * head2) #:: lazyExecuteHelper(tail1, tail2, '*')
        case (head1 #:: tail1, head2 #:: tail2, '/') => (head1 / head2) #:: lazyExecuteHelper(tail1, tail2, '/')
        case (LazyList(), _, _) => lazyList2Helper ++: LazyList()
        case (_, LazyList(), _) => lazyList1Helper ++: LazyList()
    }

    lazyExecuteHelper(lazyList1, lazyList2, operatorSign)
  }

  //Zad 3
  def duplicate[A](list: List[A], howToDuplicateList: List[Int]): List[A] = {
    def duplicateHelper(listH: List[A], howToDuplicateListH: List[Int], howMany: Int): List[A] = {
      (listH, howToDuplicateListH, howMany) match
        case (Nil, _, _) => Nil
        case (_, Nil, _) => Nil
        case (head1 :: tail1, head2 :: tail2, -1) => duplicateHelper(listH, howToDuplicateListH, head2)
        case (head1 :: tail1, head2 :: tail2, 0) => duplicateHelper(tail1, tail2, -1)
        case (head1 :: tail1, head2 :: tail2, _) => head1 :: duplicateHelper(listH, howToDuplicateListH, howMany - 1)
    }

    if howToDuplicateList == Nil then throw new Exception("The List 'How to Duplicate' is empty!")
    else duplicateHelper(list, howToDuplicateList, howToDuplicateList.head)
  }

  //Zad 4 i 5
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Dog(dogsRace: String, dogsAge: Int) extends Debug {
    var race: String = dogsRace
    var age: Int = dogsAge
  }

  trait Debug {
    def debugName(): String = this.getClass.getSimpleName

    def debugVars(): List[Any] = {
      this.getClass.getDeclaredFields.toList.map(field =>
        field.setAccessible(true) //can not access a member of class Lista6$Point with modifiers "private"
        (field.getName, field.getType, field.get(this).toString))
    }
  }

  def main(args: Array[String]): Unit = {
    //Zad 1
    println("Zad 1:")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1, 4, 5, 6, 7, 8, 9), 2, 10).toList)

    //Zad 2
    println("\nZad 2:")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '/').toList)
    println(lazyExecute(LazyList(), LazyList(), '+').toList)

    //Zad 3
    println("\nZad 3:")
    println(duplicate(List(1, 2, 3), List(0, 3, 1, 4)))
    println(duplicate(List("Filip", "Marian", "Mariola"), List(3, 0, 4)))
    println(duplicate(List("Filip", "Marian", "Mariola", "Martyna"), List(0, 0, 4)))
    // exception println(duplicate(List("Filip","Marian","Mariola"),List()))

    //Zad 4
    println("\nZad 4:")
    var p: Point = new Point(3, 4)
    var dog: Dog = new Dog("German Shepherd", 5)
    println(p.debugName())
    println(dog.debugName())



    //Zad 5
    println("\nZad 5:")
    println(p.debugVars())
    println(dog.debugVars())
  }
}

