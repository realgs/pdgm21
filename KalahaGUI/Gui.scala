package com.example.KalahaGUI

import com.example.GamePackage.Game
import com.example.ServerPackage.Server
import com.example.UserPackage.{Computer, User}
import java.awt.event.ActionEvent
import java.awt.{BorderLayout, GridLayout, Label}
import javax.swing.{JButton, JFrame, JPanel, JTextField, JTextPane, WindowConstants}
import javax.swing.text.{SimpleAttributeSet, StyleConstants}

class Gui {

  private val buttonVersion1 = new JButton("Computer - Computer")
  private val buttonVersion2 = new JButton("USER - Computer")
  private val buttonVersion3 = new JButton("USER - USER")
  private val buttonPlayer1 = new JButton("Player 1")
  private val buttonPlayer2 = new JButton("Player 2")

  private val panelWest = new JPanel()
  private val panelEast = new JPanel()
  private val panelNorth = new JPanel()
  private val panelCenter = new JPanel()

  private val user1TextInput = new JTextField()
  private val user2TextInput = new JTextField()

  private val user1Label = new Label()
  private val user2Label = new Label()

  private val frame = new JFrame
  private val gameWindow = new JTextPane()

  def buildGameLayout() = {

    panelNorth.setLayout(new GridLayout(1, 3))
    panelNorth.add(buttonVersion1)
    panelNorth.add(buttonVersion2)
    panelNorth.add(buttonVersion3)
    frame.add(panelNorth, BorderLayout.NORTH)

    panelEast.setLayout(new GridLayout(3, 1))
    user1Label.setText("      User 1")
    panelEast.add(user1Label)
    panelEast.add(user1TextInput)
    user1TextInput.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
    panelEast.add(buttonPlayer1)
    panelEast.setVisible(false)
    frame.add(panelEast, BorderLayout.EAST)

    panelWest.setLayout(new GridLayout(3, 1))
    user2Label.setText("      User 2")
    panelWest.add(user2Label)
    panelWest.add(user2TextInput)
    user2TextInput.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
    panelWest.add(buttonPlayer2)
    panelWest.setVisible(false)
    frame.add(panelWest, BorderLayout.WEST)

    panelCenter.setLayout(new GridLayout(1, 1))
    panelCenter.add(gameWindow)
    val attribs = new SimpleAttributeSet
    StyleConstants.setAlignment(attribs, StyleConstants.ALIGN_CENTER)
    gameWindow.setEditable(false)
    gameWindow.setParagraphAttributes(attribs, true)
    frame.add(panelCenter, BorderLayout.CENTER)

    buttonVersion1.addActionListener(
      (e: ActionEvent) => playOfBots(gameWindow, panelEast))
    buttonVersion2.addActionListener(
      (e: ActionEvent) => playOfOneHumenUser(buttonPlayer1, user1TextInput, gameWindow, panelEast))
    buttonVersion3.addActionListener(
      (e: ActionEvent) => playOfTwoHumanUsers(buttonPlayer1, buttonPlayer2, user1TextInput, user2TextInput, gameWindow, panelEast, panelWest))

    frame.setSize(600, 200)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    frame.setVisible(true)
  }

  def playOfBots(gameMessageOutput: JTextPane, user1Panel: JPanel): Unit = {
    val game = new Game(Game.createBoardArray(6), Game.getFirstPlayer())
    val player1 = new Computer(game, gameMessageOutput)
    val player2 = new Computer(game, gameMessageOutput)
    player1.setEnemy(player2)
    player2.setEnemy(player1)
    val server = new Server(game, gameMessageOutput)
    server.startGame()
    server.processGame(player1)
  }

  def playOfOneHumenUser(userButton1: JButton, user1TextInput: JTextField, gameMessageOutput: JTextPane, user1Panel: JPanel): Unit = {
    user1Panel.setVisible(true)

    val game = new Game(Game.createBoardArray(6), Game.getFirstPlayer())
    val player1 = new User(userButton1, user1TextInput, game, gameMessageOutput)
    val player2 = new Computer(game, gameMessageOutput)
    player1.setEnemy(player2)
    player2.setEnemy(player1)
    val server = new Server(game, gameMessageOutput)
    server.startGame()
  }

  def playOfTwoHumanUsers(userButton1: JButton, userButton2: JButton, user1TextInput: JTextField, user2TextInput: JTextField, gameMessageOutput: JTextPane, user1Panel: JPanel, user2Panel: JPanel): Unit = {
    user1Panel.setVisible(true)
    user2Panel.setVisible(true)

    val game = new Game(Game.createBoardArray(6), Game.getFirstPlayer())
    val player1 = new User(userButton1, user1TextInput, game, gameMessageOutput)
    val player2 = new User(userButton2, user2TextInput, game, gameMessageOutput)
    player1.setEnemy(player2)
    player2.setEnemy(player1)
    val server = new Server(game, gameMessageOutput)
    server.startGame()
  }
}
