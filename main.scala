package test

import boards.Board.Board
import players.Player.{Player, SimulationPlayer}
import game.Game.*

object main {
  def main(args: Array[String]): Unit = {
    Simulation()
    //SimualtionVSHuman()
    AIVSHuman()

  }

}
