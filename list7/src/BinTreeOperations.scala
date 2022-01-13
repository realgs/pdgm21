import ParallelResearchUtils.*

object BinTreeOperations {

  // Przy klasycznym liczeniu sumy elementów drzewa drzewa binarnego zdecydowana przewaga rozwiązania jednowątkowego 
  // dla głębokości 15 program zrównoleglony osiagnął wynik 78ms, niezrównoleglony 0ms
  // w celu sztucznego wydłużenia obliczeń z każdego elementu liczyłam silnię,
  // co przy dużych liczbach i drzewach o głębokości większej od 10 dawało bardzo niewielką przewagę algorytmowi równoległemu 
  // głebokośc 15, np. równoległy 1266ms, zwykły 1281ms
  // problem raczej nie nadaje się do rozwiązania równoległego
  
  sealed trait BinTree[+A]
  case object Empty extends BinTree[Nothing]
  case class Node[+A](elem: A, left: BinTree[A], right: BinTree[A]) extends BinTree[A]

  val rand = scala.util.Random()

  def generateTreeOfDepth(depth: Int): BinTree[Int] = {
    if depth <= 0 then Empty
    else
      Node(Math.abs(rand.nextInt()) % 100000, generateTreeOfDepth(depth - 1), generateTreeOfDepth(depth - 1))
  }

  def sumOfTreeFactorial(tree: BinTree[Int]): Int ={
    tree match{
      case Node(v, left, right) => factorial(v) + factorial(sumOfTree(left))+ factorial(sumOfTree(right))
      case Empty => 0
    }
  }

  def sumOfTreeFactorialParallel(tree: BinTree[Int], depthOfTree: Int): Int = {
    if depthOfTree < 10 then sumOfTree(tree)
    else
    tree match{
      case Node(v, left, right) =>
        val result = parallel(sumOfTreeParallel(left, depthOfTree - 1), sumOfTreeParallel(right, depthOfTree - 1))
        (factorial(v) + factorial(result._1) + factorial(result._2))
      case Empty => 0
    }
  }

  def sumOfTree(tree: BinTree[Int]): Int ={
    tree match{
      case Node(v, left, right) => v + sumOfTree(left)+ sumOfTree(right)
      case Empty => 0
    }
  }

  def sumOfTreeParallel(tree: BinTree[Int], depthOfTree: Int): Int = {
    if depthOfTree < 10 then sumOfTree(tree)
    else
      tree match{

        case Node(v, left, right) =>
          val result = parallel(sumOfTreeParallel(left, depthOfTree - 1), sumOfTreeParallel(right, depthOfTree - 1))
          (v + result._1 + result._2)
        case Empty => 0
      }
  }

  def factorial(number: Int): Int = {
    def factorialHelper(numberTail: Int, acc: Int): Int = {
      numberTail match {
      case 0 => 1
      case 1 => 1
      case _ => factorialHelper (numberTail - 1, acc * numberTail)
      }
    }
    if number < 0 then throw new IllegalArgumentException()
    else factorialHelper(number, 1)
  }  
}
