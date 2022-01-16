class RandomPlayer(first: Boolean) extends Player(first){

  override def makeMove(board: Board): Int =
    var house = -1

    while (!board.checkHouse(house, first))
      if first then house = scala.util.Random.nextInt(6)
      else house = scala.util.Random.nextInt(6) + 7

    println(house)
    house


}
