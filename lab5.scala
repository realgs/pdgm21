import scala.annotation.tailrec
import scala.util.Random

val toDigits: ((Int, Int) => List[Int]) = (number, base) =>
    @tailrec
    def toDigitsTailrec(number: Int, accumulator: List[Int]): List[Int] =
        if number == 0 then accumulator
        else toDigitsTailrec(number / base, (number % base) :: accumulator)
    toDigitsTailrec(number, Nil)

toDigits(31, 16) == List(1, 15)
toDigits(31, 10) == List(3, 1)
toDigits(27, 2) == List(1, 1, 0, 1, 1)

sealed trait BinaryTree[+A]
case object Leaf extends BinaryTree[Nothing]
case class Node[+A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

val randomTree: (Int => BinaryTree[Int]) = N =>
    if N <= 0 then Leaf
    else 
        val r = new Random
        Node(r.nextInt(100), randomTree(N-1), randomTree(N-1))

val t = randomTree(3)

val multiplyTree: (BinaryTree[Int] => Int) = tree =>
    tree match
        case Leaf => 1
        case Node(value, left, right) => value * multiplyTree(left) * multiplyTree(right)

multiplyTree(t)

val removeDuplicatesPreorder: (BinaryTree[Int] => BinaryTree[Int]) = tree =>
    def liftLeaf(tree: BinaryTree[Int], foundValue: Int): (BinaryTree[Int], Int) =  // liniowa wzgledem glebokosci drzewa, pamieciowa liniowa
        tree match
            case Leaf => (Leaf, foundValue)
            case Node(v, Leaf, Leaf) => (Leaf, v)
            case Node(v, Leaf, right) =>
                val (changedRight, found) = liftLeaf(right, -1)
                (Node(v, Leaf, changedRight), found)
            case Node(v, left, right) => 
                val (changedLeft, found) = liftLeaf(left, -1)
                (Node(v, changedLeft, right), found)
    @tailrec
    def replaceDuplicate(tree: BinaryTree[Int], values: List[Int]): (BinaryTree[Int], List[Int]) =  // liftLeaf() * ilosc powtorzen, pamieciowa liniowa wzgledem ilosci powtorzen
        val (affectedTree, foundValue) = liftLeaf(tree, -1)
        if values.contains(foundValue) then replaceDuplicate(affectedTree, values)
        else 
            affectedTree match
                case Leaf => (Leaf, values)
                case Node(_, t1, t2) => (Node(foundValue, t1, t2), foundValue :: values)
    def removeDuplicatesPreorderHelper(tree: BinaryTree[Int], values: List[Int]): (BinaryTree[Int], List[Int]) =  // liniowa wzgledem ilosci wezlow + usuwanie duplikatow, pamieciowa liniowa wzgledem glebokosci
        tree match
            case Leaf => (Leaf, values)
            case Node(value, left, right) =>
                if values.contains(value) then
                    val (repairedTree, newValues) = replaceDuplicate(tree, values)
                    repairedTree match
                        case Leaf => (Leaf, newValues)
                        case Node(v, t1, t2) =>
                            val (leftCorrectTree, newValues2) = removeDuplicatesPreorderHelper(t1, newValues)
                            val (rightCorrectTree, newValues3) = removeDuplicatesPreorderHelper(t2, newValues2)
                            (Node(v, leftCorrectTree, rightCorrectTree), newValues3)
                else 
                    val (leftCorrectTree, newValues2) = removeDuplicatesPreorderHelper(left, value :: values)
                    val (rightCorrectTree, newValues3) = removeDuplicatesPreorderHelper(right, newValues2)
                    (Node(value, leftCorrectTree, rightCorrectTree), newValues3)
    removeDuplicatesPreorderHelper(tree, Nil)._1

removeDuplicatesPreorder(Node(11,Node(78,Node(5,Leaf,Leaf),Node(49,Leaf,Leaf)),Node(11,Node(25,Leaf,Leaf),Node(26,Leaf,Leaf)))) == Node(11,Node(78,Node(5,Leaf,Leaf),Node(49,Leaf,Leaf)),Node(25,Leaf,Node(26,Leaf,Leaf)))
removeDuplicatesPreorder(Node(1,Node(1,Node(1,Leaf,Leaf),Node(1,Leaf,Leaf)),Node(1,Node(1,Leaf,Leaf),Node(1,Leaf,Leaf)))) == Node(1,Leaf,Leaf)
removeDuplicatesPreorder(Node(11,Node(78,Node(5,Leaf,Leaf),Node(49,Leaf,Leaf)),Node(11,Leaf,Node(26,Leaf,Leaf)))) == Node(11,Node(78,Node(5,Leaf,Leaf),Node(49,Leaf,Leaf)),Node(26,Leaf,Leaf))

// val removeDuplicatesBreadth: (BinaryTree[Int] => BinaryTree[Int]) = tree =>
//     def liftLeaf(tree: BinaryTree[Int], foundValue: Int): (BinaryTree[Int], Int) =
//         tree match
//             case Leaf => (Leaf, foundValue)
//             case Node(v, Leaf, Leaf) => (Leaf, v)
//             case Node(v, Leaf, right) =>
//                 val (changedRight, found) = liftLeaf(right, -1)
//                 (Node(v, Leaf, changedRight), found)
//             case Node(v, left, right) => 
//                 val (changedLeft, found) = liftLeaf(left, -1)
//                 (Node(v, changedLeft, right), found)
//     def replaceDuplicate(tree: BinaryTree[Int], values: List[Int]): (BinaryTree[Int], List[Int]) =
//         val (affectedTree, foundValue) = liftLeaf(tree, -1)
//         if values.contains(foundValue) then replaceDuplicate(affectedTree, values)
//         else 
//             affectedTree match
//                 case Leaf => (Leaf, values)
//                 case Node(_, t1, t2) => (Node(foundValue, t1, t2), foundValue :: values)
//     def removeDuplicatesBreadthHelper(queue: List[BinaryTree[Int]], values: List[Int]): List[Int] =
//         queue match
//             case Nil => values
//             case Leaf => removeDuplicatesBreadthHelper(queue.tail, values)
//             case Node(v, t1, t2) =>
//                 t1 match
//                     case Node(t1_v, _, _) => 
//                         if values.contains(t1_v) then 
//                             val (repairedTree, newValues) = replaceDuplicate(t1, values)
//                             queue.head.left = repairedTree
//                 t2 match
//                     case Node(t2_v, _, _) =>
//                         if (values ::: List(queue.head.left.value)).contains(t2_v) then
//                             val (repairedTree, newValues) = replaceDuplicate(t2, values ::: List(queue.head.left.value))
//                             queue.head.right = repairedTree
//                 queue.head match
//                     case Node(vv, tt1, tt2) => removeDuplicatesBreadthHelper(queue.tail ::: List(tt1, tt2), values)
//     tree match
//         case Leaf => Leaf
//         case Node(v, t1, t2) =>
//             removeDuplicatesBreadthHelper(List(tree), List(v))
//             tree

// removeDuplicatesBreadth(Node(11,Node(78,Node(5,Leaf,Leaf),Node(49,Leaf,Leaf)),Node(11,Node(25,Leaf,Leaf),Node(26,Leaf,Leaf))))