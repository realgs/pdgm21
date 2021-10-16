object Main {
  def sum(list: List[Int]): Int = {
    if list == Nil then return 0
    else return list.head + sum(list.tail)
  }

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 3, 6)))
    println(sum(List(-4, 89, 1, -56)))
  }
}
