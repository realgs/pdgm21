import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.Random

object list7 {

  def summrizeArrays(arrays: Array[Array[Int]]): Int ={
    @tailrec
    def summrize(count: Int, array: Array[Int]): Int ={
      if array.isEmpty then count
      else summrize(count+array.head, array.tail)
    }
    def sumOfAllArrays(count: Int, arrays: Array[Array[Int]]): Int ={
      if arrays.isEmpty then count
      else if arrays.size==1 then{
        val a1 =summrize(0, arrays.head)
        count + a1
      }
      else if arrays.size==2 then{
        val a1 =summrize(0, arrays.head)
        val a2 =summrize(0, arrays(1))
        count+a1+a2
      }
      else{
        val a1 =summrize(0, arrays.head)
        val a2 =summrize(0, arrays(1))
        val a3 =summrize(0, arrays(2))
        sumOfAllArrays(a1+a2+a3, arrays.tail.tail.tail)
      }
    }
    sumOfAllArrays(0, arrays)
  }

  def summrizeArraysParallel(arrays: Array[Array[Int]]): Int ={
    @tailrec
    def summrize(count: Int, array: Array[Int]): Int ={
      if array.isEmpty then count
      else summrize(count+array.head, array.tail)
    }
    def sumOfAllArrays(count: Int, arrays: Array[Array[Int]]): Int ={
      if arrays.isEmpty then count
      else if arrays.size==1 then{
        val a1 =summrize(0, arrays.head)
        count + a1
      }
      else if arrays.size==2 then{
        val a1 = Future { summrize(0, arrays.head) }
        val a2 =Future { summrize(0, arrays(1)) }
        val r1 =Await.result(a1, Duration.Inf)
        val r2 =Await.result(a2, Duration.Inf)
        count+r1+r2
      }
      else{
        val a1 =Future{ summrize(0, arrays.head) }
        val a2 =Future{ summrize(0, arrays(1)) }
        val a3 =Future{ summrize(0, arrays(2)) }
        val r1 =Await.result(a1, Duration.Inf)
        val r2 =Await.result(a2, Duration.Inf)
        val r3 =Await.result(a3, Duration.Inf)
        sumOfAllArrays(r1+r2+r3, arrays.tail.tail.tail)
      }
    }
    sumOfAllArrays(0, arrays)
  }

  def randomArray(size: Int): Array[Int] ={
    val array = new Array[Int](size)
    @tailrec
    def randomArrayInner(index: Int, array: Array[Int]):Array[Int]={
      if index<size then {
        val rnd = new scala.util.Random
        array(index) =rnd.nextInt( (100) + 1 )
        randomArrayInner(index+1, array)
      }
      else array
    }
    randomArrayInner(0, array)
  }
  def randomArrays(size:Int, sizeOfArray: Int): Array[Array[Int]]={
    val array = new Array[Array[Int]](size)
    @tailrec
    def randomArraysInner(index:Int, array: Array[Array[Int]]): Array[Array[Int]]={
      if index<size then {
        var a = randomArray(sizeOfArray)
        array(index) = a
        randomArraysInner(index+1, array)
      }
      else array
    }
    randomArraysInner(0, array)
  }

  def quickSortParallel(array: Array[Int], low: Int, high: Int): Unit={
    if(low<high) then{
      var (i, j) = partition(array, low, high)
      if(j-low < high-i) then{
        val a1 = Future{quickSort(array, low, j)}
        val a2 = Future{quickSort(array, i, high)}
        val r1 =Await.result(a1, Duration.Inf)
        val r2 =Await.result(a2, Duration.Inf)
      }
      else{
        val a1 = Future{quickSort(array, i, high)}
        val a2 = Future{quickSort(array, low, j)}
        val r1 =Await.result(a1, Duration.Inf)
        val r2 =Await.result(a2, Duration.Inf)
      }
    }
  }

  def swap(array: Array[Int], i: Int, j: Int): Unit={
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }

  def partition(array: Array[Int], low: Int, high: Int): (Int, Int)={
    var i = low
    var j = high
    val pivot = array((i+j)/2)
    while(i<=j) do{
      while(array(i)<pivot) do i=i+1
      while(array(j)>pivot) do j=j-1
      if(i<=j) then{
        swap(array, i, j)
        i=i+1
        j=j-1
      }
    }
    (i, j)
  }

  def quickSort(array: Array[Int], low: Int, high: Int): Unit={
    if(low<high) then{
      var (i, j) = partition(array, low, high)
      if(j-low < high-i) then{
        quickSort(array, low, j)
        quickSort(array, i, high)
      }
      else{
        quickSort(array, i, high)
        quickSort(array, low, j)
      }
    }
  }


  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  def createTree(height: Int, start:Int, end:Int): BT[Int] ={
    var random = new scala.util.Random
    val value = start+ random.nextInt( (end - start) + 1 )
    if height>1 then Node(value, createTree(height-1, start, value), createTree(height-1, value, end))
    else if height==0 then Empty
    else Node(value, Empty, Empty)
  }

  def DFS(bt: BT[Int]):List[Int]={
    def DFSHelper(node: BT[Int]): (List[Int]) ={
      if node==Empty then Nil
      else{
        node match{
          case Node(elem, left, right) =>List.concat( List(elem), DFSHelper(left), DFS(right))
        }
      }
    }
    DFSHelper(bt)
  }

  def DFSParallel(bt: BT[Int]):List[Int]={
    def DFSHelper(node: BT[Int]): (List[Int]) ={
      if node==Empty then Nil
      else{
        node match{
          case Node(elem, left, right) =>{
            var bt1 = Future{DFSHelper(left)}
            var bt2 = Future{DFSHelper(right)}
            val r1 =Await.result(bt1, Duration.Inf)
            val r2 =Await.result(bt2, Duration.Inf)
            List.concat( List(elem), r1, r2)
          }
        }
      }
    }
    DFSHelper(bt)
  }

  def ElapsedTime[T](function: =>T): Unit ={
    val t0 = System.nanoTime()
    val result = function
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    //println(result)
  }



  def main(args: Array[String]): Unit = {
    val arrays = randomArrays(1000, 1000)
    println("Sum of Arrays to parallel:")
    println("")
    println("Without parallel")
    ElapsedTime(summrizeArrays(arrays))
    println("With parrallel")
    ElapsedTime(summrizeArraysParallel(arrays))
    val arraysToNotParallel = randomArrays(5, 100)
    println("")
    println("")
    println("Sum of Arrays not to parallel:")
    println("")
    println("Without parallel")
    ElapsedTime(summrizeArrays(arraysToNotParallel))
    println("With parrallel")
    ElapsedTime(summrizeArraysParallel(arraysToNotParallel))

    println("-------------------------------------------------")

    val arrayToSort = randomArray(1000)
    println("Sorting arrays to parallel:")
    println("")
    println("Without parrallel")
    ElapsedTime(quickSort(arrayToSort.clone(), 0, arrayToSort.size-1))
    println("With parrallel")
    ElapsedTime(quickSortParallel(arrayToSort.clone(), 0, arrayToSort.size-1))
    val arrayToSortNoParralel = randomArray(10)
    println("")
    println("")
    println("Sorting arrays not to parallel:")
    println("")
    println("Without parrallel")
    ElapsedTime(quickSort(arrayToSortNoParralel.clone(), 0, arrayToSortNoParralel.size-1))
    println("With parrallel")
    ElapsedTime(quickSortParallel(arrayToSortNoParralel.clone(), 0, arrayToSortNoParralel.size-1))

    println("-------------------------------------------------")
    var bt = createTree(5, 0, 100)
    println("DFS to parallel:")
    println("")
    println("Without parrallel")
    ElapsedTime(DFS(bt))
    println("With parrallel")
    ElapsedTime(DFSParallel(bt))
    println("")
    println("")
    var btNotToParallel = createTree(9, 0, 100)
    println("DFS not to parallel:")
    println("")
    println("Without parrallel")
    ElapsedTime(DFS(btNotToParallel))
    println("With parrallel")
    ElapsedTime(DFSParallel(btNotToParallel))

  }

}
