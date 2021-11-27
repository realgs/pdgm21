object List5 {

    sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

    def int_to_hex(number: Int): List[Int] = {
        def iter(num: Int, list: List[Int]): List[Int] =
            if num == 0
                then list
                else iter(num / 16, (num % 16) :: list)
        iter(number, Nil)
    }

    def int_to_base(number: Int, base: Int): List[Int] = {
        def iter(num: Int, list: List[Int]): List[Int] =
            if num == 0
                then list
                else iter(num / base, (num % base) :: list)
        iter(number, Nil)
    }

    def generate_random_tree(depth: Int): BT[Double] = {
        if depth == 0
            then Empty
            else Node(scala.util.Random.nextDouble(), generate_random_tree(depth - 1), generate_random_tree(depth - 1))
    }

    def tree_product(tree: BT[Double]): Double = {
        tree match
            case Node(value, left, right) => value * tree_product(left) * tree_product(right)
            case Empty => 1
    }

    def main(args: Array[String]): Unit = {
        println("test 1:")
        println(int_to_hex(16) == List(1, 0))
        println(int_to_hex(1234) == List(4, 13, 2))
        println(int_to_hex(513) == List(2, 0, 1))

        println("test 2:")
        println(int_to_base(9, 2) == List(1, 0, 0, 1))
        println(int_to_base(7, 28) == List(7))
        println(int_to_base(895, 7) == List(2, 4, 1, 6))

        println("test 3 & 4:")
        println(1 == tree_product(Empty))
        val tree1 = generate_random_tree(1)
        val Node(v, _, _) = tree1
        println(v == tree_product(tree1))
        val tree2 = generate_random_tree(2)
        val Node(v1, Node(v2, _, _), Node(v3, _, _)) = tree2
        println(v1 * v2 * v3 == tree_product(tree2))
    }
}