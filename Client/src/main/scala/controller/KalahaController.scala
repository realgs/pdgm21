package controller

import controller.actionlistenersimpl.{ConnectButtonActionListener, JoinGameButtonActionListener, StartGameButtonActionListener, BoardActionListener, ShowPlayersButtonActionListener}
import controller.animation.MoveAnimation
import controller.connection.KalahaClientConnection
import status.GameStatus
import view.KalahaGuiCreator

import javax.swing.{JButton, JTextField}

class KalahaController(gui: KalahaGuiCreator, game: GameStatus):
    var gameStatus: GameStatus = game
    var moveAnimation: MoveAnimation = new MoveAnimation(gui.boardPanel);
    val connection: KalahaClientConnection = new KalahaClientConnection(this)

    var connectButton: JButton = gui.controlsPanel.connectionPanel.connectButton
    var showPlayersButton: JButton = gui.controlsPanel.playersPanel.showPlayersButton
    var nameSubmitButton: JButton = gui.controlsPanel.namePanel.nameSubmitButton
    var nameTextField: JTextField = gui.controlsPanel.namePanel.nameTextField
    var startGameButton: JButton = gui.controlsPanel.gamePanel.startGameButton

    connectButton.addActionListener(new ConnectButtonActionListener(connection))
    showPlayersButton.addActionListener(new ShowPlayersButtonActionListener(connection))
    nameSubmitButton.addActionListener(new JoinGameButtonActionListener(connection, this))
    startGameButton.addActionListener(new StartGameButtonActionListener(connection))

    def onConnected(): Unit =
        gui.controlsPanel.connectionPanel.connectStatus.setText("Connected!")
        gui.controlsPanel.connectionPanel.connectButton.setEnabled(false)

    def onRegistered(): Unit =
        println("Registered succesfully")

    def onPlayers(firstPlayerName: String, secondPlayerName: String): Unit =
        gameStatus.playerNames(0) = firstPlayerName
        gameStatus.playerNames(1) = secondPlayerName
        gui.controlsPanel.playersPanel.firstPlayerLabel.setText(firstPlayerName)
        gui.controlsPanel.playersPanel.secondPlayerLabel.setText(secondPlayerName)
        val boardButtons: Array[JButton] = (if firstPlayerName == gameStatus.name then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
        for (i <- boardButtons.indices)
            boardButtons(i).addActionListener(new BoardActionListener(connection, i))

    def onGameStarted(): Unit =
        println("Game started!")
        val boardButtons: Array[JButton] = (if gameStatus.name == gameStatus.playerNames(0) then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
        for (i <- boardButtons.indices)
            boardButtons(i).setEnabled(true)

    def onRemaining(remaining: String): Unit =
        gui.controlsPanel.gamePanel.timeoutLabel.setText(s"Time: $remaining")

    def onTimeIsUp(): Unit =
        gui.controlsPanel.gamePanel.timeoutLabel.setText("You lose!")

    def onMoveMade(playerName: String, holeNumber: Int): Unit =
        println("gameStatus: " + playerName + " " + gameStatus.playerNames(0))
        val playerNum = if playerName == gameStatus.playerNames(0) then 0 else 1
        moveAnimation.animate(playerNum, holeNumber)
        println("sending animationDone")
        connection.commandAnimationDone()
