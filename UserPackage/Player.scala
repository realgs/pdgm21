package com.example.UserPackage

import com.example.GamePackage.Game
import com.example.ServerPackage.Server
import javax.swing.{JTextPane}

class Player(private val game: Game, private val gamePane: JTextPane) {
  val server = new Server(game, gamePane)

  def moveRequest(board: Game): Unit = {}

  def setEnemy(user2: Player): Unit = {}
}
