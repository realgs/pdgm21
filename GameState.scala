package pdgm21



case class GameState (
                  val nextPlayer:Int,  //player1 :1 playe2:2
                  val moveEndCode:Int,  //not started: -2, badMove:-1, good move: 0 , win: playerId
                  val gameBoard:Array[Int])