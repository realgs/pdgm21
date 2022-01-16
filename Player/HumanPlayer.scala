package Kalaha.Player

import Kalaha.Player.Player.*

import scala.io.StdIn.readLine

object HumanPlayer {

  class HumanPlayer() extends Player() :

    override def chooseNextMove(): Int =
      println("Choose your next move : (choose pit number from 1 to 6)")

      var nextMove = readLine()
      while !goodInt(nextMove) do
        println("Such a pit doesn't exist ! Choose again !")
        nextMove = readLine()

      nextMove.toInt - 1

    private def goodInt(input: String): Boolean =
      input match
        case "1" => true
        case "2" => true
        case "3" => true
        case "4" => true
        case "5" => true
        case "6" => true
        case _ => false

    override def makeMove(lastMove: Int): Unit = ()
}
