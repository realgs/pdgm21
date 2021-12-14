import java.util
import scala.List

object Main {
  def main(args: Array[String]): Unit = {
    println("--------- 1")
    val list = LazyList(5,6,3,2,1)
    val result = eachNElement(list,3,2).toList
    println(result)

    println()

    val result2 = eachNElement(list,4,2).toList
    println(result2)

    println()

    val result3 = eachNElement(list, -1, 2).toList
    println(result3)

    println()

    val result4 = eachNElement(list, 5, -1).toList
    println(result4)


    println("------------------------")




    println("--------- 2")
    val first = LazyList(1,2,3)
    val second = LazyList(2,3,4,5)
    val addResult = lazyExecute(first,second,'+')
    println(addResult.toList)

    println()

    val first1 = LazyList()
    val second1 = LazyList(1,2,3,4,5)
    val divResult = lazyExecute(first1, second1, '/')
    println(divResult.toList)

    println()

    val first2 = LazyList(2,2,2,2)
    val second2 = LazyList(1,0,-1)
    val mulResult = lazyExecute(first2, second2, '*')
    println(mulResult.toList)

    println("------------------------")
    println("--------- 3")

    val listToDuplicate = List(1,2,3)
    val duplicators = List(0,3,1,4)

    val duplicatedResult = duplicate(listToDuplicate,duplicators)
    println(duplicatedResult)

    println()

    val listToDuplicate1 = List("A","B","C")
    val duplicators1 = List(1,2)

    val duplicatedResult1 = duplicate(listToDuplicate1,duplicators1)
    println(duplicatedResult1)

    println("------------------------")
    println("--------- 4/5")


    var p = new Point(3,4)
    println(p.debugName())
    println()
    println(p.debugVars())

    println("------------------------")

  }
}


def eachNElement[A](list :LazyList[A], end :Int, gap :Int) =
  def eachNElementHelper[A](list :LazyList[A], endElem :Int, currentGap :Int): LazyList[A] =
    (list,endElem) match
      case (LazyList(),_) => LazyList()
      case (h #:: t, 0) => LazyList()
      case (h #:: t,_) =>
        if currentGap == 1 then h #:: eachNElementHelper(t, endElem - 1, gap)
        else  eachNElementHelper(t, endElem - 1, currentGap - 1)


  if end >= 0 && gap > 0 then eachNElementHelper(list, end, 1)
  else LazyList()





def lazyExecute(first: LazyList[Int], second: LazyList[Int], symbol :Char) =
  symbol match
    case '+' => (first.zipAll(second,0,0)).map((f,s) => f + s)
    case '-' => (first.zipAll(second,0,0)).map((f,s) => f - s)
    case '*' => (first.zipAll(second,0,0)).map((f,s) => f * s)
    case '/' => (first.zipAll(second,0,0)).map((f,s) => f / s)
    case _ => LazyList()


def copy[A](element: A, number: Int): List[A] =
  if number > 0 then element :: copy(element, number-1)
  else Nil


def duplicate[A](list: List[A], copyNumbers :List[Int]): List[A] =
  (list zip copyNumbers).flatMap((a,n) => copy(a,n))





trait Debug {
  def debugName() = this.getClass().getName().toString

  def debugVars1() =
    val fields = this.getClass().getDeclaredFields.toList
    val fieldsNames = fields.map(f => f.getName().toString)
    val fieldsClass = fields.map(f => f.getAnnotatedType().toString)
    fields.foreach(f => f.setAccessible(true))
    val fieldsValues = fields.map(f => f.get(this))

    fieldsNames.lazyZip(fieldsClass).lazyZip(fieldsValues).toList

  def debugVars() = this.getClass().getDeclaredFields.toList
    .map(f =>
      f.setAccessible(true)
      List(f.getName(), f.getAnnotatedType().toString, f.get(this))
    )

}


class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}








