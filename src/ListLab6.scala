import java.lang.reflect.Field
import scala.::

object ListLab6 {
  def main(args: Array[String]): Unit = {

//    Zadanie 1
    println("ZADANIE 1")
    println(eachNElement(LazyList(5,6,3,2,1),2,3).force)
    println(eachNElement(LazyList(5,6,3,2,1),2,4).force)
    println(eachNElement(LazyList(5,6,3,2,1),2,5).force)
    println(eachNElement(LazyList(5,6,3,2,1),2,0).force)
    println(eachNElement(LazyList(5,6,3,2,1),0,5).force)
    println(eachNElement(LazyList(),2,5).force)

//    Zadanie 2
   println("\nZADANIE 2")
   println(lazExecute(LazyList(5,6,3),LazyList(5,6,1,2,1),add).force)
   println(lazExecute(LazyList(5,6,3),LazyList(5,6,1,2,1),subtract).force)
   println(lazExecute(LazyList(5,6,3),LazyList(2,3,1,2,1),multiply).force)
   println(lazExecute(LazyList(5,6,3),LazyList(2,3,1,2,1),division).force)
   println(lazExecute(LazyList(2,0,0),LazyList(0,3,1,2,1),division).force)

//    Zadanie 3
println("\nZADANIE 3")
println(duplicate(LazyList(5,6,3),LazyList(2,3,1,2,1)).force)
println(duplicate(LazyList(5,6,3),LazyList(0,0,0,2,1)).force)

//    Zadanie 4
println("\nZADANIE 4")
val point = new Point(2,4)
println(point.debugVars)
println(point.debugName)
}
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
    def helper(list: LazyList[A], k: Int, m: Int):LazyList[A] =
      if k<m then list(k)#::helper(list,k+n,m) else LazyList()
    if list!=Nil && n > 0 && m >= 0 then list(0)#::helper(list,n,m) else LazyList()


  def lazExecute2(list_a: LazyList[Double],list_b: LazyList[Double], sign: Char): LazyList[Double] =
    if sign.equals('+') then lazExecute(list_a,list_b,add)
    else if sign.equals('-') then lazExecute(list_a,list_b,subtract)
    else if sign.equals('*') then lazExecute(list_a,list_b,multiply)
    else if sign.equals('/') then lazExecute(list_a,list_b,division)
    else LazyList()

  def lazExecute(list_a: LazyList[Double],list_b: LazyList[Double], action: (Double,Double)=>Double): LazyList[Double] =
      if list_a == Nil then list_b
      else if list_b == Nil then list_a
      else action(list_a.head,list_b.head)#::lazExecute(list_a.tail,list_b.tail,action)

  def duplicate[A](listOfElements: LazyList[A],listOfNUmbers: LazyList[Int]): LazyList[A] =
    def helper(elem: A, n: Int): LazyList[A] =
      if n != 0 then elem#::helper(elem,n-1) else duplicate(listOfElements.tail,listOfNUmbers.tail)
    if listOfElements == LazyList() || listOfNUmbers == LazyList() then LazyList() else helper(listOfElements.head,listOfNUmbers.head)

  def add(x: Double, y: Double): Double = x+y
  def subtract(x: Double, y: Double): Double = x-y
  def multiply(x: Double, y: Double): Double = x*y
  def division(x: Double, y: Double): Double = if y==0 then 0 else x/y

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  trait Debug{
    def debugName: String = this.getClass().toString
    def fieldTOList(fs: List[java.lang.reflect.Field]): List[List[String]] =
      if fs.isEmpty then List() else
        fs.head.setAccessible(true)
        List(fs.head.getName.toString,  fs.head.getType.toString,  fs.head.get(this).toString)::fieldTOList(fs.tail)
    def debugVars: List[Any] = fieldTOList(this.getClass.getDeclaredFields.toList)
  }



}
