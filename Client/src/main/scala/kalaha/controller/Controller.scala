package kalaha.controller

import kalaha.view.animation.MoveAnimation
import kalaha.connection.Connection
import kalaha.service.Service
import kalaha.status.GameStatus
import kalaha.view.GuiCreator

import java.awt.Color
import java.awt.event.ActionEvent
import javax.swing.{JButton, JTextField}

class Controller(val gui: GuiCreator, val gameStatus: GameStatus):
    val service: Service = new Service(gui)
    var moveAnimation: MoveAnimation = new MoveAnimation(gui.boardPanel);
    val connection: Connection = new Connection(this)

    service.connectButton.addActionListener((e: ActionEvent) => connection.connectToServer)

    def onConnected: Unit =
        service.onConnected
        service.showPlayersButton.addActionListener((e: ActionEvent) => connection.commandGetPlayers)
        service.nameSubmitButton.addActionListener((e: ActionEvent) =>
            val typedName = service.nameTextField.getText
            if !typedName.endsWith("AI") && typedName != gameStatus.playerNames(0) && typedName != gameStatus.playerNames(1) then
                gameStatus.name = typedName
                connection.commandJoinGame
                connection.commandGetPlayers
        )
        service.startGameButton.addActionListener((e: ActionEvent) => connection.commandStartGame)

    def onRegistered: Unit =
        service.onRegistered

    def onPlayers(name0: String, name1: String): Unit =
        gameStatus.playerNames(0) = name0
        gameStatus.playerNames(1) = name1
        service.onPlayers(name0, name1)
        val boardButtons: Array[JButton] = (if name0 == gameStatus.name then service.firstPlayerHoles else service.secondPlayerHoles)
        for i <- boardButtons.indices do
            boardButtons(i).addActionListener((e: ActionEvent) =>
                val myButtons: Array[JButton] = if gameStatus.playerNames(0) == gameStatus.name then service.firstPlayerHoles else service.secondPlayerHoles
                for b <- myButtons do
                    b.setEnabled(false)
                connection.commandMakeMove(i)
            )

    def onGameStarted: Unit =
        service.onGameStarted

    def onRemaining(remaining: String): Unit =
        service.onRemaining(remaining)

    def onTimeIsUp(loser: String): Unit =
        service.gamePanel.timeoutLabel.setText(s"Time is up, you ${if loser == gameStatus.name then "lose :(" else "win :)"}!")
        connection.socket.close

    def onMoveMade(playerName: String, holeNumber: Int): Unit =
        val playerNum = if playerName == gameStatus.playerNames(0) then 0 else 1
        moveAnimation.animate(playerNum, holeNumber)
        connection.commandAnimationDone

    def onMoveAllowed(): Unit =
        val yourBoardButtons: Array[JButton] = (if gameStatus.playerNames(0) == gameStatus.name then service.firstPlayerHoles else service.secondPlayerHoles)
        for button <- yourBoardButtons if button.getText != "0" do
            button.setEnabled(true)

    def onGameOver(winner: String): Unit =
        service.onGameOver(winner)
        connection.socket.close