import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Main {

  //task 1
  def eachNElement[A](list: LazyList[A], offset: Int, endIndx: Int): LazyList[A] = {
    def getNextElem(count: Int, offsetCount: Int, listTail: LazyList[A]) : LazyList[A] = {
      if count >= endIndx then LazyList()
      else if offsetCount == 0 then listTail.head #:: getNextElem(count + 1, offset - 1, listTail.tail)
      else getNextElem(count + 1, offsetCount - 1, listTail.tail)
    }
    if offset > 0 then getNextElem(0, 0, list)
    else LazyList()
  }

  //task 2
  def lazyExecute(firstl: LazyList[Int], secl: LazyList[Int], oper: Char) : LazyList[Int] = {
    def executeIn(f: LazyList[Int], s: LazyList[Int]) : LazyList[Int] = {
      (firstl, secl) match {
        case (h1 #:: t1, h2 #::t2) =>
          oper match{
            case '+' => (h1 + h2) #:: lazyExecute(t1, t2, oper)
            case '-' => (h1 - h2) #:: lazyExecute(t1, t2, oper)
            case '*' => (h1 * h2) #:: lazyExecute(t1, t2, oper)
            case '/' => if h2 != 0 then (h1 / h2) #:: lazyExecute(t1, t2, oper)
                        else throw new ArithmeticException("Division by zero")
          }
        case (LazyList(), h2 #::t2) => secl
        case (h1 #:: t1, LazyList()) => firstl
        case _ => LazyList()
      }
    }
    if oper == '+' || oper == '-' || oper == '*' || oper == '/' then executeIn(firstl, secl)
    else throw new UnsupportedOperationException()
  }

  //task 3
  def duplicate[A](listElems: LazyList[A], listNum: LazyList[Int]) : LazyList[A] = {
    def repeatElem(elem: A, count: Int, l1: LazyList[A], l2: LazyList[Int]): LazyList[A] = {
      if count == 1 then elem #:: nextElem(l1, l2)
      else elem #:: repeatElem(elem, count - 1, l1, l2)
    }
    def nextElem(listElemIn: LazyList[A], listNumIn: LazyList[Int]): LazyList[A] = {
      (listElemIn, listNumIn) match {
        case (h1 #:: t1, h2 #:: t2) =>
          if h2 <= 0 then nextElem(t1, t2)
          else repeatElem(h1, h2, t1, t2)
        case (_, LazyList()) => listElemIn
        case _ =>  LazyList()
      }
    }
    nextElem(listElems, listNum)
  }

  //task 3 ArrayBuffer
  def duplicateAB[A](list: ArrayBuffer[A], howManyList: ArrayBuffer[Int]): ArrayBuffer[A] ={
    var elemIndex = 0
    var multIndex = 0

    while(elemIndex < list.size && multIndex < howManyList.size){
      if howManyList(multIndex) <= 0 then
        list.remove(elemIndex)
        multIndex += 1
      else
        val currentElem = list(elemIndex)
        var counter = howManyList(multIndex)
        while(counter > 1){
          list.insert(elemIndex + 1, currentElem)
          elemIndex += 1
          counter -= 1
        }
        elemIndex += 1
        multIndex += 1
    }
    list
  }

  //task 4-5
  trait Debug{
    def debugName(): String = getClass.getSimpleName

    def debugVars(): List[List[String]]= {
      var fields :Array[java.lang.reflect.Field] = this.getClass.getDeclaredFields
      var buffer = ListBuffer[List[String]]()

      for( f <- fields){
        f.setAccessible(true)
        buffer += List(f.getName, f.getType.toString, f.get(this).toString)
      }
      buffer.toList
    }
  }

  def main(args: Array[String]): Unit ={
    val l = LazyList(1,2,3,4,5,6,7,8,9)
    val m = LazyList(5,6,3,2,0)

    //task 1
            println(eachNElement(l, 3, 9).toList)
            println(eachNElement(m, 2, 3).toList)
            println(eachNElement(m, 3, 5).toList)
    //task2
            println(lazyExecute(l, m, '+').toList)
            println(lazyExecute(l, m, '*').toList)
            //println(lazyExecute(l, m, '/').toList)
            println(lazyExecute(l, m, '-').toList)
            //println(lazyExecute(l, m, '%').toList)

    //task 3
    val list1 = LazyList(1,2,3)
    val listNumbers = LazyList(0,3,1,4)

    var list2 = LazyList(1,2,3,4,8,9)
    var listNumbers2 = LazyList(0,3,1,4,-2)

    println(duplicate(list1, listNumbers).toList)
    println(duplicate(list2, listNumbers2).toList)


    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "test"
    }

    var p : Point = new Point(3, 4);
    var d : Point = new Point(0, 4);
    //task 4
    println(p.debugName());
    println(d.debugName());
    //task 5
    println(p.debugVars());
    println(d.debugVars());

  }


}
