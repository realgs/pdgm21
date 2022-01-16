package Kalaha.Player

import Kalaha.Player.Player.*

import scala.util.Random

object RandomComputerPlayer {

  class RandomComputerPlayer() extends Player() :

    override def getNextMove(): Int =
      Random.between(0, 6)

    override def makeMove(lastMove: Int): Unit = ()
}
