object l8:
  def main(args: Array[Int]): Unit =
    val game = Game()
    game.move(false, 3)
    game.printBoard()
    game.move(true, 3)
    game.printBoard()
    game.move(false, 0)
    game.printBoard()
    game.move(true, 5)
    game.printBoard()
