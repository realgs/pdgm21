import scala.annotation.tailrec
object l5 {

  //zadanie 1
  def decimalToHex(dec: Int):List[Int] = {
    @tailrec
    def decimalToHexHelper(decNum: Int, hexNum: List[Int]): List[Int] =
      decNum match
        case 0 => hexNum
        case _ => decimalToHexHelper(decNum/16, (decNum%16)::hexNum)
    decimalToHexHelper(dec, List())
  }

  //zadanie 2
  def decimalToSystem(dec: Int, system: Int):List[Int] = {
    @tailrec
    def decimalToSystemHelper(decNum: Int, hexNum: List[Int]):List[Int] =
      decNum match
        case 0 => hexNum
        case _ => decimalToSystemHelper(decNum/system, (decNum%system)::hexNum)
    decimalToSystemHelper(dec,List())
  }

  //zadanie 3
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val random = scala.util.Random

  def generateBT(depth: Int):BT[Float] = {
     depth match
      case 0 => Empty
      case _ => Node(random.nextFloat(), generateBT(depth - 1), generateBT(depth - 1))
  }

//zadanie 4
  def multiplyBTNodes(bt: BT[Float]):Float = {
    bt match {
      case Empty => 1
      case Node(value, left, right) => value*multiplyBTNodes(left)*multiplyBTNodes(right)
    }
  }

  def breadthBT(bt: BT[Float]):List[Float] = {
    def breadth(queue: List[BT[Float]]):List[Float] = {
      queue match {
        case Nil => Nil
        case Empty::t => breadth(t)
        case Node(value, left, right)::t => value::breadth(t:::List(left,right))
      }
    }
    breadth(List(bt))
  }

  def depthBT(bt:BT[Float]):List[Float] = {
    def depth(stack: List[BT[Float]]):List[Float] = {
      stack match {
        case Nil => Nil
        case Empty::t => depth(t)
        case Node(value, left, right)::t => value::depth(List(left, right):::t)
      }
    }
    depth(List(bt))
  }

  def main(args: Array[String]): Unit = {

    println(decimalToHex(31))
    println(decimalToHex(1128))
    println(decimalToSystem(31,16))
    println(decimalToSystem(1128, 16))
    println(decimalToSystem(15,2))
    println(decimalToSystem(15,3))
    println(decimalToSystem(15,4))
    println()
    val bt = generateBT(3)
    println(breadthBT(bt))
    println(depthBT(bt))
    println(multiplyBTNodes(bt))
  }
}
