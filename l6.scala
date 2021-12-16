import scala.collection.mutable.Queue

object Main {
  def eachNElement[A](n: Int, list: LazyList[A], m: Int): LazyList[A] = {
    def eachNElementHelper[A](currentIndex: Int, lastIndex: Int, list: LazyList[A], currentDiff: Int, maxDiff: Int): LazyList[A] = {
      if (currentIndex >= lastIndex) then LazyList()
      else if (currentDiff == 0) then list.head #:: eachNElementHelper(currentIndex+1, lastIndex, list.tail, maxDiff-1, maxDiff)
      else eachNElementHelper(currentIndex+1, lastIndex, list.tail, currentDiff-1, maxDiff)
      
    }
    eachNElementHelper(0, m, list, n-1, n)
  }
  
  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operator: Char) : LazyList[Int] = {
    (list1, list2) match 
      case (head1 #:: tail1, head2 #:: tail2) =>
        operator match 
          case '+' => (head1 + head2) #:: lazyExecute(tail1, tail2, operator)
          case '-' => (head1 - head2) #:: lazyExecute(tail1, tail2, operator)
          case '*' => (head1 * head2) #:: lazyExecute(tail1, tail2, operator)
          case '/' => (head1 / head2) #:: lazyExecute(tail1, tail2, operator)
          case _ => throw new RuntimeException("Unsupported Operation")
        
      case (head1#::tail1, LazyList()) => list1
      case (LazyList(), head2#::tail2) => list2
      case _ => LazyList()
    
  }
  
  
  
  def duplicate[A](elementsToDuplicate: Queue[A], factors: Queue[Int]): Queue[A] ={
    val elements = elementsToDuplicate.clone()
    var currentFactor = 0
    val result = Queue[A]()

    while(!elements.isEmpty && currentFactor < factors.size){
      if factors(currentFactor) <= 0 then
        elements.dequeue
        currentFactor += 1
      else
        var currentElement = elements.dequeue
        var counter = factors(currentFactor)
        while(counter > 0){
          result.enqueue(currentElement)
          counter -= 1
        }
        currentFactor += 1
    }
    result
  }
  
  
  trait Debug {
    def debugName() = 
      this.getClass().getName()
    
    def debugVars() =
      val list = this.getClass.getDeclaredFields.toList
      def debugVarsHelper(list: List[java.lang.reflect.Field]): List[List[Any]] =
          list match
            case head :: tail =>
              head.setAccessible(true)
              List(head.getName(), head.getType(), head.get(this)) :: debugVarsHelper(tail)
            case _ => List()
      debugVarsHelper(list)
      
  }
  
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }
  
  
  def main(args: Array[String]): Unit ={
    val test1 = LazyList(1, 4, 8, 13, 15, 20, 26, 39, 123, 150, 155, 1029)
    
    println(eachNElement(3, test1, 12).toList)
    println(eachNElement(2, test1, 12).toList)
    println(eachNElement(1, test1, 4).toList)
    
    val test2 = LazyList(1, 3, 6, 15, 21)
    
    println(lazyExecute(test1, test2, '+').toList)
    
    val test3 = LazyList(1, 65, 123, 0, 124)

 // throws ArithmeticException   println(lazyExecute(test2, test3, '/').toList)
    
    val test4 = LazyList(1, 2, 3, 0, 1)
    
    val queue1 = Queue(1, 2, 3)
    val queue2 = Queue(1, 0, 3)
    println(duplicate(queue1, queue2))
    
    
    var p : Point = new Point(3, 4)
    println(p.debugName()) 
    
    println(p.debugVars())
    
    println("End")
  }
  
  
}
