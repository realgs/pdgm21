package packMain
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {

    // zad 1+2
    def numberToList(number:Int, base:Int): List[Int] =
      @tailrec
      def starter(n:Int): Int =
        if n<=number then starter(n*base) else n/base
      @tailrec
      def lister(n:Int, number:Int, acc:List[Int]): List[Int] =
        if n == 0 then acc else lister(n/base, number-n*(number/n), (number/n)::acc)
      lister(starter(1), number, List()).reverse

    // zad 3
    sealed trait Tree[+A]
    case class Leaf[A](value: A) extends Tree[A]
    case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
    val rand = scala.util.Random
    def generateTree(N:Int): Tree[Double] =
      if N == 1 then Leaf(rand.nextDouble())
      else Node(rand.nextDouble(), generateTree(N-1), generateTree(N-1))

    // zad 4
    def multiplication(t:Tree[Double]): Double =
      t match
        case l:Leaf[Double] => l.value
        case n:Node[Double] => n.value*multiplication(n.left)*multiplication(n.right)



    // testy
    println("\nZadanie 1+2:")
    println(numberToList(31, 16))
    println(numberToList(1024, 2))
    println(numberToList(57, 8))
    def treePrinterD(t:Tree[Double], h:Int): Boolean =
      @tailrec
      def space(h:Int, acc:String): String = if h==0 then acc else space(h-1, " "+acc)
      t match
        case n: Node[Double] =>
          print(space(h, ""))
          println(n.value)
          treePrinterD(n.left, h+1)
          treePrinterD(n.right, h+1)
          true
        case l: Leaf[Double] =>
          print(space(h, ""))
          println(l.value)
          true
    println("\nZadanie 3")
    val testTree = generateTree(4)
    val testTree2 = Node(
      0.1,
      Node(
        0.2,
        Node(
          0.3,
          Leaf(0.4),
          Leaf(0.5)
        ),
        Leaf(0.6)
      ),
      Node(
        0.7,
        Leaf(0.8),
        Node(
          0.9,
          Leaf(1.1),
          Leaf(1.2)
        )
      )
    )
    treePrinterD(testTree, 0)
    println("\nZadanie 4 (dla tego samego drzewa")
    println(multiplication(testTree))
    println("\n dla innego drzewa o postaci:")
    treePrinterD(testTree2, 2)
    print("Iloczyn (powinno być 0,0004790016): ")
    println(multiplication(testTree2)) // powinno być 0,0004790016
  }
}
