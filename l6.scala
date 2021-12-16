
object l6 {

  def eachNElement[A](list: LazyList[A], n: Int, m: Int):LazyList[A] = {
    def eachNElementHelper[A](llist: LazyList[A], index: Int):LazyList[A] = {
      if(index == m) then LazyList()
      else
        llist match
          case LazyList() => LazyList()
          case h#::tail =>
            if index % n == 0 then h#::(eachNElementHelper(tail,index+1))
            else eachNElementHelper(tail,index+1)
    }
    eachNElementHelper(list,0)
  }

  def lazyExecute(list1: LazyList[Float], list2: LazyList[Float], opertor: Char):LazyList[Float] = {
    def lazyExecuteHelper(llist1: LazyList[Float], llist2: LazyList[Float]):LazyList[Float] = {
      (llist1,llist2) match
        case(LazyList(),LazyList()) => LazyList()
        case (_, LazyList()) => llist1
        case (LazyList(),_) => llist2
        case(h1#::t1,h2#::t2) => operation(h1,h2,opertor)#::lazyExecuteHelper(t1,t2)
    }
    lazyExecuteHelper(list1,list2)
  }

  def operation(a: Float, b: Float, operator: Char): Float = {
    operator match
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case '/' =>
        if b!=0 then a/b else throw new Exception("Can't divide by 0")
      case _ => throw new Exception("Wrong operator")
  }

  def repeat[A](listElements: LazyList[A],listReps: LazyList[Int]): LazyList[A] = {
    def repeatHelper(restElements: LazyList[A], restReps: LazyList[Int]):LazyList[A] = {
      (restElements,restReps) match {
        case (h1#::t1,0#::t2) => repeatHelper(t1,t2)
        case (h1#::_, h2#::t2) => h1#::repeatHelper(restElements, (h2-1)#::t2)
        case (_,_) => LazyList()
      }
    }
    repeatHelper(listElements,listReps)
  }

  trait Debug {

    def debugName():String =
      getClass.getSimpleName

    def debugVars():List[List[Any]] =
      getClass.getDeclaredFields.toList.map(field => {
        field.setAccessible(true)
        List(field.getName(), field.getType(), field.get(this))
      })
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {
    println(eachNElement(LazyList(5,6,3,2,1),2,3).toList)
    println(eachNElement(LazyList(5,6,3,2,1),2,4).toList)
    println(eachNElement(LazyList(1,2,3,4,5,6,7,8,9),3,8).toList)

    println(lazyExecute(LazyList(1,2,3),LazyList(1,2,3,4,5,6),'+').toList)
    println(lazyExecute(LazyList(1,2,3),LazyList(1,2,3,4,5,6),'-').toList)
    println(lazyExecute(LazyList(1,2,3),LazyList(1,2,3,4,5,6),'*').toList)
    println(lazyExecute(LazyList(1,2,3),LazyList(2,1,3,4,5,6),'/').toList)

    println(repeat(LazyList(1,2,3),LazyList(0,3,1,4)).toList)
    println(repeat(LazyList(1,2,3),LazyList(3)).toList)
    println(repeat(LazyList(),LazyList(3)).toList)

    var p:Point = new Point(3,4)
    println(p.debugName())
    println(p.debugVars())

  }

}
