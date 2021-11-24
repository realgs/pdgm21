object List5 {

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

    def main(args: Array[String]): Unit = {
        println("test 1:")
        println(int_to_hex(16) == List(1, 0))
        println(int_to_hex(1234) == List(4, 13, 2))
        println(int_to_hex(513) == List(2, 0, 1))
        println("test 2:")
        println(int_to_base(9, 2) == List(1, 0, 0, 1))
        println(int_to_base(7, 28) == List(7))
        println(int_to_base(895, 7) == List(2, 4, 1, 6))
    }
}