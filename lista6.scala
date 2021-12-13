import java.lang.reflect.Field;
object l6 {

  def main(args: Array[String]): Unit = {
    println("ZADANIE 1")
    println(eachNElement(LazyList(5,6,3,2,1),2,3).toList)
    println(eachNElement(LazyList(5,6,3,2,1),2,4).toList)
    println("ZADANIE 2")
    println(lazyExecute(LazyList(1,2,3), LazyList(2,3,4,5), +).toList)
    println("ZADANIE 3")
    println(duplicateElements(LazyList(1,2,3), LazyList(0,3,1,4)).toList)
    println("ZADANIE 4")
    var p : Point = new Point(3, 4);
    println(p.debugName());
    println("ZADANIE 5")
    println(p.debugVars())

  }

  //zadanie 1
  def eachNElement(baseList:LazyList[Int], n:Int, m:Int): LazyList[Int] = {
    def eachNElementHelper(list:LazyList[Int], index:Int, endIndex:Int, result:LazyList[Int]): LazyList[Int] = {
      (list, index%n, endIndex) match
        case (LazyList(), _, _) => result
        case (_, _, 0) => result
        case (h#::t, 0, _) => eachNElementHelper(t, index+1,endIndex-1, h#::result)
        case (h#::t, _, _) => eachNElementHelper(t,index+1,endIndex-1, result)
    }
    eachNElementHelper(baseList, 0, m, LazyList()).reverse
  }

  //zadanie 2
  def lazyExecute(firstList: LazyList[Int], secondList: LazyList[Int], operation: (Int, Int) => Int): LazyList[Int] = {
    def lazyExecuteHelper(firstListHelper: LazyList[Int], secondListHelper: LazyList[Int], resultList: LazyList[Int]): LazyList[Int] = {
      (firstListHelper, secondListHelper) match {
        case (LazyList(), _) => secondListHelper#:::resultList
        case (_, LazyList()) => firstListHelper#:::resultList
        case (h1#::t1, h2#::t2) => lazyExecuteHelper(t1,t2,operation(h1,h2) #:: resultList)
      }
    }
    lazyExecuteHelper(firstList,secondList, LazyList()).reverse
  }

  def + (a:Int, b:Int) = a+b;
  def - (a:Int, b:Int) = a-b;
  def * (a:Int, b:Int) = a*b;
  def / (a:Int, b:Int) = a/b;


  //zadanie 3
  def duplicateElements(baseList: LazyList[Int], duplicateList: LazyList[Int]): LazyList[Int] = {
    def duplicateElementsHelper(baseListHelp: LazyList[Int], duplicateListHelp: LazyList[Int], resultList: LazyList[Int]): LazyList[Int] = {
      (baseListHelp, duplicateListHelp) match
        case (LazyList(), _) => resultList
        case (_, LazyList()) => resultList
        case (hb#::tb, hd#::td) => duplicateElementsHelper(tb, td, repeatElement(hb,hd)#:::resultList)
    }
    duplicateElementsHelper(baseList, duplicateList, LazyList()).reverse
  }

  def repeatElement(elem:Int, repeats:Int): LazyList[Int] = {
    def repeatElementHelper(elemHelp:Int, repeatsHelp:Int, resultList:LazyList[Int]): LazyList[Int] = {
      repeatsHelp match
        case 0 => resultList
        case _ => repeatElementHelper(elemHelp, repeatsHelp-1, elemHelp#::resultList)
    }
    if repeats < 0 then LazyList()
    else repeatElementHelper(elem, repeats, LazyList())
  }

}

//zadanie 4 i 5
trait Debug {
  def debugName() =
    this.getClass.getName

  def debugVars() = {
    def debugHelper(field: Field) = {
      val fieldName = field.getName
      val fieldsType = field.getType
      val f = this.getClass.getDeclaredField(fieldName);
      f.setAccessible(true)
      val fieldValue = f.get(this)

      List(fieldName, fieldsType, fieldValue)
    }
    this.getClass.getDeclaredFields.map(debugHelper).toList
  }

}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}
