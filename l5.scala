import scala.annotation.tailrec

object List5 {

    sealed trait BT[+A]
    case object Empty extends BT[Nothing]
    case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

    def int_to_hex(number: Int): List[Int] = {
        @tailrec
        def iter(num: Int, list: List[Int]): List[Int] =
            if num == 0
                then list
                else iter(num / 16, (num % 16) :: list)
        if number <= 0 then List() else iter(number, Nil)
    }

    def int_to_base(number: Int, base: Int): List[Int] = {
        @tailrec
        def iter(num: Int, list: List[Int]): List[Int] =
            if num == 0
                then list
                else iter(num / base, (num % base) :: list)
        if number <= 0 then List() else iter(number, Nil)
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

    def reverse[A](list: List[A]): List[A] = {
        @tailrec
        def iter(list: List[A], acc: List[A]): List[A] = {
            if list == Nil
                then acc
                else iter(list.tail, list.head :: acc)
        }
        iter(list, Nil)
    }

    def filter[A](list: List[A], predicate: A => Boolean): List[A] = {
        @tailrec
        def iter(list: List[A], acc: List[A]): List[A] = {
            if list == Nil
                then acc
                else
                    if predicate(list.head)
                        then iter(list.tail, list.head :: acc)
                        else iter(list.tail, acc)
        }
        reverse(iter(list, Nil))
    }
    
    def make_tree[A](list: List[(A, Int)], index: Int): BT[A] = {
        filter(list, (element, id) => id == index) match
            case Nil => Empty
            case (element, id) :: t =>
                Node(
                    element,
                    make_tree(list, index * 2),
                    make_tree(list, index * 2 + 1)
                    )
    }
    
    @tailrec
    def reduce_indexes[A](list: List[(A, Int)], result: List[(A, Int)], indexes: Set[Int]): List[(A, Int)] = {
        list match
            case Nil => reverse(result)
            case (element, 1) :: t => reduce_indexes(t, (element, 1) :: result, indexes + 1)
            case (element, index) :: t =>
                if indexes(index / 2)
                    then reduce_indexes(t, (element, index) :: result, indexes + index)
                    else reduce_indexes((element, index / 2) :: t, result, indexes)
    }

    @tailrec
    def remove_duplicates[A](list: List[(A, Int)], result: List[(A, Int)], duplicates: Set[A]): List[(A, Int)] = {
        list match
            case Nil => reverse(result)
            case (element, index) :: t =>
                if duplicates(element)
                    then remove_duplicates(t, result, duplicates)
                    else remove_duplicates(t, (element, index) :: result, duplicates + element)
    }
    
    def remove_duplicates_bfs[A](tree: BT[A]): BT[A] = {
        @tailrec
        def bfs(subtree: (BT[A], Int), queue: List[(BT[A], Int)], result: List[(A, Int)]): List[(A, Int)] = {
            (subtree, queue) match
                case ((Node(v, left, right), id), h :: t) =>
                    bfs(h, t ::: List((left, id * 2), (right, id * 2 + 1)), (v, id) :: result)
                case ((Node(v, left, right), id), Nil) =>
                    bfs((left, id * 2), List((right, id * 2 + 1)), (v, id) :: result)
                case ((Empty, id), h :: t) =>
                    bfs(h, t, result)
                case ((Empty, id), Nil) =>
                    reverse(result)
        }
        val tmp1 = bfs((tree, 1), Nil, Nil)
        val tmp2 = remove_duplicates(tmp1, Nil, Set())
        val tmp3 = reduce_indexes(tmp2, Nil, Set())
        make_tree(tmp3, 1)
    }

    def remove_duplicates_dfs[A](tree: BT[A]): BT[A] = {
        def dfs(subtree: BT[A], result: List[(A, Int)], index: Int): List[(A, Int)] = {
            subtree match
                case Node(v, left, right) => (v, index) :: dfs(left, Nil, index * 2) ::: dfs(right, Nil, index * 2 + 1)
                case Empty => Nil
        }
        val tmp1 = dfs(tree, Nil, 1)
        val tmp2 = remove_duplicates(tmp1, Nil, Set())
        val tmp3 = reduce_indexes(tmp2, Nil, Set())
        make_tree(tmp3, 1)
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

        println("test 5:")
        println(remove_duplicates_dfs(Empty) == Empty)
        println(remove_duplicates_bfs(Empty) == Empty)
        val tree3 =
            Node(1,
                Node(1,
                    Node(6, Empty, Empty),
                    Node(5, Empty, Empty)),
                Node(5,
                    Node(6, Empty, Empty),
                    Node(4, Empty, Empty))
                )
        println(remove_duplicates_dfs(tree3) == Node(1, Node(6, Empty, Node(5, Empty, Empty)), Node(4, Empty, Empty)))
        println(remove_duplicates_bfs(tree3) == Node(1, Node(6, Empty, Empty), Node(5, Empty, Node(4, Empty, Empty))))
        val tree4 =
            Node(6,
                Node(3,
                    Node(4,
                        Node(1, Empty, Empty),
                        Node(5, Empty, Empty)
                    ),
                    Node(6,
                        Node(6, Empty, Empty),
                        Empty
                    )
                ),
                Node(3,
                    Node(2,
                        Node(3, Empty, Empty),
                        Node(1, Empty, Empty)
                    ),
                    Node(6,
                        Node(3, Empty, Empty),
                        Empty
                    )
                )
            )
        println(remove_duplicates_bfs(tree4) == remove_duplicates_dfs(tree4))
    }
}
