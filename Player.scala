abstract class Player(val Nr1: Boolean) {
  protected var game = KalahaSim()

  def moveOwn(): Int       // requests poz of next move
  def move(poz: Int, Nr1:Boolean): Unit =     // updates board
    game.move(poz, Nr1) 
  def noMove(): Boolean = game.noMoveLeft(Nr1)      // checks if player can make a move
  def score(): Int = game.points(Nr1)       // checks the score of the player
  def test(): Unit = Interface.showBoard(game)
  def updateGame(board:KalahaSim): Unit =
    game = board.cloner()
  def skipFix(): Unit  
}
