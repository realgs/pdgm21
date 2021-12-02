import scala.annotation.tailrec

object Lista5 {

  def contains[A](list: List[A], number: A): Boolean =
    (list, number) match
      case (Nil, _) => false
      case (_, _) =>
        if list.head == number then true
        else contains(list.tail, number)

  //Zadanie 1
  def hex(number: Int): List[Int] =
    @tailrec
    def hexIn(n: Int, result: List[Int]): List[Int] =
      if n == 0 then result
      else hexIn(n / 16, (n % 16) :: result)

    if number == 0 then List(0)
    else hexIn(number, Nil)

  //2
  def randomSystem(number: Int, system: Int): List[Int] =
    @tailrec
    def randomSystemIn(n: Int, s: Int, result: List[Int]): List[Int] =
      if n == 0 then result
      else randomSystemIn(n / s, s, (n % s) :: result)

    if number == 0 then List(0)
    else randomSystemIn(number, system, Nil)

  //Drzewa
  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


  //Zadanie 3
  val random = scala.util.Random

  def createBT(N: Int): BT[Double] =
    N match
      case 0 => Node(random.nextDouble(), Empty, Empty)
      case _ => Node(random.nextDouble(), createBT(N - 1), createBT(N - 1))


  //Zadanie 4
  def mul(bt: BT[Double]): Double =
    @tailrec
    def mulIn(btList: List[BT[Double]], result: Double): Double =
      btList match
        case Nil => result
        case Empty :: tail => mulIn(tail, result)
        case Node(v, l, r) :: tail => mulIn(tail ::: List(l, r), v * result)

    mulIn(List(bt), 1)


  //Zadanie 5
  def breadthNoDuplicate[A](bt: BT[A]) =
    def breadthNoDuplicateIn[A](visited: List[A], queue: List[BT[A]]): List[A] =
      queue match
        case Nil => Nil
        case Empty :: tail => breadthNoDuplicateIn(visited, tail)
        case Node(v, l, r) :: tail =>
          if (contains(visited, v)) then breadthNoDuplicateIn(visited, tail)
          else v :: breadthNoDuplicateIn(v :: visited, tail ::: List(l, r))

    breadthNoDuplicateIn(Nil, List(bt))


  def breadthBT[A](bt: BT[A]) =
    def breadthIn[A](queue: List[BT[A]]): List[A] =
      queue match
        case Nil => Nil
        case Empty :: tail => breadthIn(tail)
        case Node(v, l, r) :: tail => v :: breadthIn(tail ::: List(l, r))

    breadthIn(List(bt))


  def depthNoDuplicate[A](bt: BT[A]): List[A] =
    def depthNoDuplicateIn[A](visited: List[A], stack: List[BT[A]]): List[A] =
      stack match
        case Nil => Nil
        case Empty :: tail => depthNoDuplicateIn(visited, tail)
        case Node(v, l, r) :: tail =>
          if (contains(visited, v)) then depthNoDuplicateIn(visited, l :: r :: tail)
          else v :: depthNoDuplicateIn(v :: visited, l :: r :: tail)

    depthNoDuplicateIn(Nil, List(bt))

  def depthBT[A](bt: BT[A]): List[A] =
    def depthIn(p: (BT[A], List[A])): List[A] =
      p match
        case (Empty, labels) => labels
        case (Node(v, t1, t2), labels) => v :: depthIn(t1, depthIn(t2, labels))

    depthIn(bt, Nil)


  def listToTree[A](list: List[A]): BT[A] =
    list match
      case Nil => Empty
      case head :: tail => val temp = tail.splitAt(tail.length / 2)
        Node(head, listToTree(temp._1), listToTree(temp._2))


  def main(args: Array[String]): Unit = {
    System.out.println("\nZadanie 1: ")
    System.out.println(hex(31))
    System.out.println(hex(0))
    System.out.println(hex(1))

    System.out.println("\nZadanie 2: ")
    System.out.println(randomSystem(31, 16))
    System.out.println(randomSystem(12, 2))
    System.out.println(randomSystem(1, 4))


    System.out.println("\nZadanie 3: ")
    val tree0 = createBT(2)
    val tree1 = createBT(5)
    System.out.println(tree0)
    System.out.println(breadthBT(tree0))
    System.out.println("\n")
    System.out.println(tree1)
    System.out.println(breadthBT(tree1))

    System.out.println("\nZadanie 4: ")
    System.out.println(mul(tree0))
    System.out.println(mul(tree1))

    //System.out.println(contains(List(1,4,5),1))

    System.out.println("\nZadanie 5: ")
    System.out.println("\nwszerz: ")
    System.out.println(breadthBT(tree1))
    System.out.println(listToTree(breadthBT(tree1)))
    System.out.println(breadthNoDuplicate(tree1))
    System.out.println(listToTree(breadthNoDuplicate(tree1)))

    System.out.println(breadthBT(tree0))
    System.out.println(listToTree(breadthBT(tree0)))
    System.out.println(breadthNoDuplicate(tree0))
    System.out.println(listToTree(breadthNoDuplicate(tree0)))

    val tree2 = Node(1,
      Node(2,
        Node(4,
          Empty,
          Empty
        ),
        Empty
      ),
      Node(3,
        Node(5,
          Empty,
          Node(2,
            Empty,
            Empty
          )
        ),
        Empty
      )
    )

    val tree3 = Node(1,
      Node(2,
        Empty,
        Node(3,
          Empty,
          Empty
        )
      ),
      Empty
    )
    System.out.println("\n")
    System.out.println(breadthBT(tree2))
    System.out.println(listToTree(breadthBT(tree2)))
    System.out.println(breadthNoDuplicate(tree2))
    System.out.println(listToTree(breadthNoDuplicate(tree2)))

    System.out.println("\n")
    System.out.println(breadthBT(tree3))
    System.out.println(listToTree(breadthBT(tree3)))
    System.out.println(breadthNoDuplicate(tree3))
    System.out.println(listToTree(breadthNoDuplicate(tree3)))

    System.out.println("\nwgłąb: ")
    System.out.println(depthBT(tree1))
    System.out.println(listToTree(depthBT(tree1)))
    System.out.println(depthNoDuplicate(tree1))
    System.out.println(listToTree(depthNoDuplicate(tree1)))

    System.out.println(depthBT(tree0))
    System.out.println(listToTree(depthBT(tree0)))
    System.out.println(depthNoDuplicate(tree0))
    System.out.println(listToTree(depthNoDuplicate(tree0)))

    System.out.println("\n")
    System.out.println(depthBT(tree2))
    System.out.println(listToTree(depthBT(tree2)))
    System.out.println(depthNoDuplicate(tree2))
    System.out.println(listToTree(depthNoDuplicate(tree2)))

    System.out.println("\n")
    System.out.println(depthBT(tree3))
    System.out.println(listToTree(depthBT(tree3)))
    System.out.println(depthNoDuplicate(tree3))
    System.out.println(listToTree(depthNoDuplicate(tree3)))
  }
}



