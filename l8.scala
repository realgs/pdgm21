object l8:
  def main(args: Array[Int]): Unit =
    val k = Kalaha()
    val game = Game()
    val chosen = k.chooseMove(game.player1, game.player2, 0, 0, 3, false)._2
    println(chosen)

