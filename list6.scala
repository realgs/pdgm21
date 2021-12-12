import java.util
import javax.xml.transform.Result
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object list6 {
//1
def eachNElement[A](lazyList: LazyList[A], n:Int, end:Int):LazyList[A]={
  @tailrec
  def eachNElementInner(lazyList: LazyList[A], index: Int, result: LazyList[A]):LazyList[A]={
    if index>end then result
    else if index%n==0 then eachNElementInner(lazyList.tail, index+1, lazyList.head#::result)
    else eachNElementInner(lazyList.tail, index+1, result)
  }
  if end > lazyList.length || end<0 then throw new Exception ("incorrect value")
  else eachNElementInner(lazyList.tail, 1,LazyList(lazyList.head)).reverse
}
//2
def lazyExecute(lazyList1: LazyList[Int], lazList2: LazyList[Int], oper: Char): LazyList[Int] ={
  @tailrec
  def lazyExecuteInner(lazyList1i: LazyList[Int], lazyList2i: LazyList[Int], result: LazyList[Int]): LazyList[Int]={
    if lazyList1i==Nil && lazyList2i==Nil then result
    else if lazyList1i==Nil then result#:::lazyList2i
    else if lazyList2i==Nil then result#:::lazyList1i
    else if oper=='+' then lazyExecuteInner(lazyList1i.tail, lazyList2i.tail, result#:::LazyList(lazyList1i.head+ lazyList2i.head))
    else if oper=='-' then lazyExecuteInner(lazyList1i.tail, lazyList2i.tail, result#:::LazyList(lazyList1i.head- lazyList2i.head))
    else if oper=='*' then lazyExecuteInner(lazyList1i.tail, lazyList2i.tail, result#:::LazyList(lazyList1i.head* lazyList2i.head))
    else if oper=='/' then lazyExecuteInner(lazyList1i.tail, lazyList2i.tail, result#:::LazyList(lazyList1i.head/ lazyList2i.head))
    else throw new Exception("incorrect operator")
  }
  lazyExecuteInner(lazyList1, lazList2, LazyList())
}
//3
def duplicate[A](original: LazyList[A], duplicating: LazyList[Int]): LazyList[A]={
  @tailrec
  def duplicateInner(original: LazyList[A], duplicating: LazyList[Int], result: LazyList[A], count: Int): LazyList[A]={
    if original==Nil then result
    else if count==0 then duplicateInner(original.tail, duplicating.tail, result, duplicating.head)
    else duplicateInner(original, duplicating, original.head#::result, count-1)
  }
  duplicateInner(original, duplicating.tail, LazyList(), duplicating.head).reverse
}

class Debug{
  //4
  def debugName(): String ={
    def debugNameInner(name: String): String={
      if name.head.equals('$') then name.tail
      else debugNameInner(name.tail)
    }
    debugNameInner(this.getClass().getName().toString())
  }
  //5
  def debugVars():ListBuffer[ListBuffer[String]] ={
    var field: Array[java.lang.reflect.Field]= this.getClass().getDeclaredFields()

    def getAll(result: ListBuffer[ListBuffer[String]]): ListBuffer[ListBuffer[String]] ={
      field.foreach(_.setAccessible(true))
      for(i<-field){
        result+=ListBuffer(i.getName().toString(),i.getType.toString(), i.get(this).toString())
      }
      field.foreach(_.setAccessible(false))
      result
    }
    getAll(ListBuffer())
  }
}
class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

  def main(args: Array[String]): Unit = {
    println(eachNElement(LazyList(3, 4, 5, 6, 1, 2), 2, 4).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 3, 9).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(7, 8, 3, 2), '*').toList)
    println(duplicate(LazyList(1, 2, 3), LazyList(0,3,1,4)).toList)
    var p : Point = new Point(3, 4);
    println(p.debugName())
    println(p.debugVars())

  }
}
