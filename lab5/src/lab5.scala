import scala.util.Random
import scala.annotation.tailrec

object lab5 {

  // Zadanie 1

  def dec_to_hex(number : Int) : List[Int] =
    @tailrec
    def helper(number : Int, list : List[Int]) : List[Int] =
      if number < 0 then Nil
      else number match
            case 0 => list
            case _ => helper(number / 16, number % 16 :: list)
    helper(number / 16, number % 16 :: List())

  //Zadanie 2

  def dec_to_system (number : Int, system : Int) : List[Int] =
    @tailrec
    def helper(number : Int, system : Int, list : List[Int]) : List[Int] =
      if number < 0 then Nil
      else number match
            case 0 => list
            case _ => helper(number / system, system, number % system :: list)
    helper(number / system, system, number % system :: List())

  //Zadanie 3

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = new Random()

  def generateTree(n : Int) : BT[Float] =
    n match
      case 0 => Empty
      case _ => Node(random.nextFloat(), generateTree(n - 1), generateTree(n - 1))


  //Zadanie 4

  def multiplyNodes(tree : BT[Float]) : Float =
    tree match
      case Node(value, left, right) => value * multiplyNodes(left) * multiplyNodes(right)
      case Empty => 1



  def treeBFS[A](tree : BT[A]) : List[A] =
    def bfs[A](queue : List[BT[A]]) : List[A] =
      queue match {
        case Node(value, left, right) :: tail => value :: bfs(tail ::: List(left, right))
        case Empty :: tail => bfs(tail)
        case Nil => Nil
      }
    bfs(List(tree))

  def main(args : Array[String]) : Unit = {
    println("Zadanie 1")
    println(dec_to_hex(31))
    println(dec_to_hex(167))
    println(dec_to_hex(-21))

    println("\nZadanie 2")
    println(dec_to_system(31, 16))
    println(dec_to_system(31, 2))

    println("\nZadanie 3 + 4")

    val tree1 = generateTree(2)
    println(treeBFS(tree1))
    println(multiplyNodes(tree1))
    println()

    val tree2 = generateTree(3)
    println(treeBFS(tree2))
    println(multiplyNodes(tree2))

  }

}
