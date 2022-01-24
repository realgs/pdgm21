import scala.io.StdIn.readInt

object l8:
  def main(args: Array[Int]): Unit =
    play()

  def simulation(): Unit =
    val k = Kalaha()
    val game = Game()
    while(!k.isGameOver(game.player1, game.player2))
      val chosen = k.chooseMove(game.player1, game.player2, 0, 0, 3, game.player1Turn)._2

      val next = if game.player1Turn then "Player 1" else "Player 2"
      println(next + " chose " + {chosen + 1})

      game.move(chosen)
      game.printBoard()

    if(game.player1Points == game.player2Points) then println("DRAW")
    else
      val winner = if game.player1Points > game.player2Points then "Player 1" else "Player 2"
      println("The winner is: " + winner)

  def play(): Unit =
    val k = Kalaha()
    val game = Game()
    game.printBoard()
    while(!k.isGameOver(game.player1, game.player2))
      if game.player1Turn then
        val chosen = k.chooseMove(game.player1, game.player2, 0, 0, 3, game.player1Turn)._2
        val next = if game.player1Turn then "Player 1" else "Player 2"
        println(next + " chose " + {chosen + 1})
        game.move(chosen)
        game.printBoard()
      else
        println("Choose house: ")
        var chosen = readInt()
        while(chosen < 0 || chosen > 6 || game.player2({chosen-1}) == 0)
          println("Choose house: ")
          chosen = readInt()
        game.move({chosen - 1})
        game.printBoard()
    if(game.player1Points == game.player2Points) then println("DRAW")
    else
      val winner = if game.player1Points > game.player2Points then "Player 1" else "Player 2"
      println("The winner is: " + winner)
