package players

import players.Player

import scala.io.StdIn.readInt

class HumanPlayer(id: Int) extends Player(id):

  def makeMove(): Int =
    readUserChoice()

  private def readUserChoice(): Int =
    var userChoice = -1

    print("Player " + id + ": Please enter number representing house(1-6): ")
    try{
      userChoice = readInt()
    }catch{
      case _ : Throwable => println("That's not an integer number")
    }
    userChoice
