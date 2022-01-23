object l8:
  def main(args: Array[Int]): Unit =
    val kalaha = Kalaha()
    val game = Game()
    var i = 0;
    while(!kalaha.isGameOver(game.player1, game.player2))
      i = i + 1
      if i % 2 == 0 then
        val chosen = kalaha.chooseMove(game.player1, game.player2, 0, 0, 5, game.player1Turn)._2
        game.move(chosen)
      else
        val chosen = kalaha.chooseMove(game.player1, game.player2, 0, 0, 5, game.player1Turn)._2
        game.move(chosen)
      game.printBoard()
