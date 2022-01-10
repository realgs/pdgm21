package controller

import controller.actionlistenersimpl.{BoardActionListener, ConnectButtonActionListener, JoinGameButtonActionListener, ShowPlayersButtonActionListener, StartGameButtonActionListener}
import view.animation.MoveAnimation
import controller.connection.KalahaClientConnection
import status.GameStatus
import view.KalahaGuiCreator

import java.awt.Color
import javax.swing.{JButton, JTextField}

class KalahaController(_gui: KalahaGuiCreator, game: GameStatus):
    val gui: KalahaGuiCreator = _gui
    var gameStatus: GameStatus = game
    var moveAnimation: MoveAnimation = new MoveAnimation(gui.boardPanel);
    val connection: KalahaClientConnection = new KalahaClientConnection(this)

    var connectButton: JButton = gui.controlsPanel.connectionPanel.connectButton
    var showPlayersButton: JButton = gui.controlsPanel.playersPanel.showPlayersButton
    var nameSubmitButton: JButton = gui.controlsPanel.namePanel.nameSubmitButton
    var nameTextField: JTextField = gui.controlsPanel.namePanel.nameTextField
    var startGameButton: JButton = gui.controlsPanel.gamePanel.startGameButton

    connectButton.addActionListener(new ConnectButtonActionListener(connection))

    def onConnected(): Unit =
        gui.controlsPanel.connectionPanel.connectStatus.setText("Connected!")
        gui.controlsPanel.connectionPanel.connectButton.setEnabled(false)

        gui.controlsPanel.namePanel.nameTextField.setEnabled(true)
        gui.controlsPanel.namePanel.nameSubmitButton.setEnabled(true)
        gui.controlsPanel.gamePanel.startGameButton.setEnabled(true)
        gui.controlsPanel.playersPanel.showPlayersButton.setEnabled(true)

        showPlayersButton.addActionListener(new ShowPlayersButtonActionListener(connection))
        nameSubmitButton.addActionListener(new JoinGameButtonActionListener(connection, this))
        startGameButton.addActionListener(new StartGameButtonActionListener(connection))

    def onRegistered(): Unit =
        gui.controlsPanel.namePanel.nameTextField.setEnabled(false)
        gui.controlsPanel.namePanel.nameSubmitButton.setEnabled(false)

    def onPlayers(firstPlayerName: String, secondPlayerName: String): Unit =
        gameStatus.playerNames(0) = firstPlayerName
        gameStatus.playerNames(1) = secondPlayerName
        gui.controlsPanel.playersPanel.firstPlayerLabel.setText(firstPlayerName)
        gui.controlsPanel.playersPanel.secondPlayerLabel.setText(secondPlayerName)
        val boardButtons: Array[JButton] = (if firstPlayerName == gameStatus.name then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
        for (i <- boardButtons.indices)
            boardButtons(i).addActionListener(new BoardActionListener(connection, this, i))

    def onGameStarted(): Unit =
        gui.controlsPanel.gamePanel.startGameButton.setEnabled(false)
        gui.controlsPanel.namePanel.nameTextField.setEnabled(false)
        gui.controlsPanel.namePanel.nameSubmitButton.setEnabled(false)
        gui.controlsPanel.playersPanel.showPlayersButton.setEnabled(false)

    def onRemaining(remaining: String): Unit =
        gui.controlsPanel.gamePanel.timeoutLabel.setText(s"Time: $remaining")

    def onTimeIsUp(loser: String): Unit =
        gui.controlsPanel.gamePanel.timeoutLabel.setText(s"Time is up, you ${if loser == gameStatus.name then "lose :(" else "win :)"}!")
        connection.socket.close()

    def onMoveMade(playerName: String, holeNumber: Int): Unit =
        val playerNum = if playerName == gameStatus.playerNames(0) then 0 else 1
        moveAnimation.animate(playerNum, holeNumber)
        connection.commandAnimationDone()

    def onMoveAllowed(): Unit =
        val yourBoardButtons: Array[JButton] = (if gameStatus.playerNames(0) == gameStatus.name then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
        for (button <- yourBoardButtons)
            if button.getText() != "0" then
                button.setEnabled(true)

    def onGameOver(winner: String): Unit =
        for (button <- gui.boardPanel.firstPlayerHoles)
            button.setEnabled(false)
            gui.boardPanel.bases(0).setText(s"${gui.boardPanel.bases(0).getText().toInt + button.getText().toInt}")
        for (button <- gui.boardPanel.secondPlayerHoles)
            button.setEnabled(false)
            gui.boardPanel.bases(1).setText(s"${gui.boardPanel.bases(1).getText().toInt + button.getText().toInt}")
        gui.boardPanel.bases(0).setBackground(Color.RED)
        gui.boardPanel.bases(1).setBackground(Color.RED)
        gui.controlsPanel.gamePanel.timeoutLabel.setText(s"Game over, winner: $winner")
        connection.socket.close()