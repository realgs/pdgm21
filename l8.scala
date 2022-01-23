object l8:
  def main(args: Array[Int]): Unit =
    val game = Game()
    game.move(true, 2)
    game.printBoard()
