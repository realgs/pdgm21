package players

import scala.io.StdIn.*
import scala.util.Random

class HumanPlayer(override val playerNum: Int) extends Player(playerNum) {

  def chooseMove(): Int = {
    var correctArg = false
    var choice = -1
    while (!correctArg) {
      println("Choose a house: ")
      try {
        choice = readInt()
        if (choice < 1 || choice > 6) {
          println("House number must between 1 and 6")
        } else {
          correctArg = true
        }
      } catch {
        case e => println("Ivalid input format, must be integer number")
      }
    }
    choice
  }
}

