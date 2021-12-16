package Lab5

import scala.annotation.tailrec

object Lab5 {

  //1

  def conversion_to_hex(decimal_number: Int): List[Int] =
    @tailrec
    def conversion_to_hex_in(rest: Int, hex_number: List[Int]): List[Int] =
      if rest == 0 then hex_number
      else conversion_to_hex_in(rest / 16, rest % 16 :: hex_number)
    if decimal_number <= 0 then List(0):::conversion_to_hex_in((-1)*decimal_number, Nil) else conversion_to_hex_in(decimal_number, Nil)

  //2

  def conversion_to_other(decimal_number: Int, new_System: Int): List[Int] =
    @tailrec
    def conversion_to_other_in(rest: Int, new_System: Int, other_number: List[Int]): List[Int] =
      if rest == 0 then other_number
      else conversion_to_other_in(rest / new_System, new_System, rest % new_System :: other_number)
    if decimal_number <= 0 then List(0):::conversion_to_other_in((-1)*decimal_number,new_System,Nil) else conversion_to_other_in(decimal_number, new_System, Nil)

  //3

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def make_tree(depth: Int): BT[Double] =
    if depth <= 0 then Empty
    else Node(math.random(), make_tree(depth-1),make_tree(depth-1))

  //4

  def multiply_node(tree: BT[Double]):Double =
    def multiply_node_in(queue: BT[Double], acc: Double):Double  =
      queue match
        case Empty => acc
        case Node(v,l,r) => multiply_node_in(l,multiply_node_in(r,v*acc))
    multiply_node_in(tree, 1.0 )


    //5

  @tailrec
  def duplicates[A](list_el: List[A], value: A): Boolean =
    list_el match
      case Nil => false
      case h::t => if h == value then true else duplicates(t, value)

  def depthBT[A](tree: BT[A]): BT[A] ={
    def depthBTTail[B](actual_node: BT[B], elements_in_tree: List[B]): (BT[B],List[B]) =
      actual_node match
        case Empty => (Empty, elements_in_tree)
        case Node(v,_,_)  if duplicates(elements_in_tree, v) => (Empty, elements_in_tree)
        case Node(v, left, right)=>
          val left_resultat = depthBTTail(left,v :: elements_in_tree)
          val right_resultat = depthBTTail(right,left_resultat._2)
          (Node(v,left_resultat._1,right_resultat._1),right_resultat._2)

    depthBTTail(tree,List())._1
  }

  def brethBT[A](Tree: BT[A]): BT[A] = {
    @tailrec
    def brethBTTail[AA](queue: List[BT[AA]], elements_in_tree: List[AA], result: List[(AA, Int, AA | Null)]): List[(AA, Int, AA | Null)] = //Rodzic, lewy/prawy/ elem
      queue match
        case Nil => result
        case Empty :: tl => brethBTTail(tl, elements_in_tree, result)
        case Node(v, left, right) :: tl =>
          (left, right) match
            case (Empty, Empty) => brethBTTail(tl, elements_in_tree, (v, 0, null) :: (v, 1, null) :: result)
            case (Node(v1, _, _), Empty) => if duplicates(elements_in_tree, v1) then brethBTTail(tl, elements_in_tree, (v, 0, null) :: (v, 1, null) :: result)
              else brethBTTail(tl ::: List(left, right), v1 :: elements_in_tree, (v, 0, v1) :: (v, 1, null) :: result)
            case (Empty, Node(v2, _, _)) => if duplicates(elements_in_tree, v2) then brethBTTail(tl, elements_in_tree, (v, 0, null) :: (v, 1, null) :: result)
              else brethBTTail(tl ::: List(left, right), v2 :: elements_in_tree, (v, 0, null) :: (v, 1, v2) :: result)
            case (Node(v1, _, _), Node(v2, _, _)) =>
              (duplicates(elements_in_tree, v1), duplicates(elements_in_tree, v2)) match
                case (true, true) => brethBTTail(tl, elements_in_tree, (v, 0, null) :: (v, 1, null) :: result)
                case (true, false) => brethBTTail(tl ::: List(left, right), v2 :: elements_in_tree, (v, 0, null) :: (v, 1, v2) :: result)
                case (false, true) => brethBTTail(tl ::: List(left, right), v1 :: elements_in_tree, (v, 0, v1) :: (v, 1, null) :: result)
                case (false, false) => brethBTTail(tl ::: List(left, right), v1 :: v2 :: elements_in_tree, (v, 0, v1) :: (v, 1, v2) :: result)


    def build_node(result: List[(A|Null, Int, A | Null)], elem: (A|Null, Int), pre: List[(A|Null, Int, A | Null)]): BT[A] =
      if result.head._1 == elem._1 && result.head._2 == elem._2 then
        if result.head._3 == null then Empty  else Node(result.head._3.asInstanceOf[A],build_node(pre,(result.head._3,0), pre),build_node(pre,(result.head._3,1), pre))
      else build_node(result.tail,elem, pre)

    val Node(v, _, _) = Tree
    val resultat = brethBTTail(List(Tree), List(v), List())
    Node(v,build_node(resultat,(v,0),resultat),build_node(resultat,(v,1),resultat))
  }

  def main(args: Array[String]): Unit = {

    println(conversion_to_hex(-31) == List(1, 15))
    println(conversion_to_hex(2121) == List(8, 4, 9))
    println(conversion_to_hex(0) == List(0))
    println(conversion_to_other(256, 2) == List(1, 0, 0, 0, 0, 0, 0, 0, 0))
    println(conversion_to_other(211, 14) == List(1, 1, 1))
    val tree = make_tree(2)
    println(tree)
    println(multiply_node(tree))


    val tt = Node(1,
      Node(4,
        Node(2,
          Node(7,Empty,Empty),
          Empty
        ),
        Empty
      ),
      Node(2,
        Node(5,
          Empty,
          Node(6,
            Empty,
            Empty
          )
        ),
        Empty
      )
    )

    println(depthBT(tt))
    println(brethBT(tt))


  }
}
