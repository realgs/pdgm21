package com.example.UserPackage

import com.example.GamePackage.Game
import java.awt.event.{ActionEvent}
import javax.swing._

class User(private val userMoveButton: JButton, private val userText: JTextField, private val game: Game, private val gamePane: JTextPane, private var enemy: Player = null) extends Player(game, gamePane) {

  override def setEnemy(user2: Player): Unit = {
    enemy = user2
  }

  override def moveRequest(board: Game): Unit = {
      userMoveButton.setText("Your turn!")
  }

  userMoveButton.addActionListener((e: ActionEvent) => {
    val userField = userText.getText()
    userText.setText("")
    userMoveButton.setText("")
    server.moveReceived(enemy, Integer.parseInt(userField))}
  )
}

