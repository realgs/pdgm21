import scala.io.StdIn.readLine

class ClientComputer(ID: String, hasCurrentTurn: Boolean) extends Client (ID, hasCurrentTurn) {

  val client1 = "P1";                                                           //players are identified as Strings
  val client2 = "P2";

  def chooseHouse(game: Game):Int = {
    var bestInput = 0
    var alternativeGame: Game = new Game()

    var counter = 0;
    var mostStonesSoFar = 0;

    if (ID == client1) {
    mostStonesSoFar = countStonesInMancala(client1, alternativeGame.getBoard());

      while (counter < 6) {
        alternativeGame = game.alternativeBoardCopy()

        if ((alternativeGame.getBoard())(counter).getStonesCount() == 0) {      //if hole is empty, increment index of a hole
          counter += 1
        } else {
          var (whoNext, stonesInMancala) = tryMove(alternativeGame, counter)
          if (whoNext == client1) {                                             //machine decides on a move that give you a second turn
            bestInput = counter
            mostStonesSoFar = 999
          } else if (stonesInMancala > mostStonesSoFar) {                                   //if number of stones in mancala is bigger then current max
            bestInput = counter
            mostStonesSoFar = stonesInMancala
          }
          counter += 1
        }
      }

    } else if (ID == client2) {
      counter = 7
      while (counter < 13) {
        alternativeGame = game.alternativeBoardCopy()                           //create an alternative game instance
        if ((alternativeGame.getBoard())(counter).getStonesCount() == 0) {
          counter += 1
        } else {
          var (whoNext, stonesInMancala) = tryMove(alternativeGame, counter)    //choose the hole thet gives you second move
          if (whoNext == client2) {
            bestInput = counter
            mostStonesSoFar = 999
          } else if (stonesInMancala > mostStonesSoFar) {
            bestInput = counter
            mostStonesSoFar = stonesInMancala
          }
          counter += 1
        }
      }
    }
    println("Client " + ID + " has chosen house "+ (bestInput + 1) +"\n")
    bestInput
  }

  def tryMove(game: Game, index: Int): (String, Int) = {                                              //returns who is next and stones in mancala
    var alternativeGame: Game = game.alternativeBoardCopy()
    ( alternativeGame.evaluateMove(index, ID), countStonesInMancala(ID, alternativeGame.getBoard()))  //evluates turn in an alternative board
  }

  def countStonesInMancala(who : String, array: Array[House]): Int = array(if who == client1 then 6 else 13).getStonesCount()




}