package controller

import cask.*
import cask.Logger.Console.globalLogger
import cask.util.WsClient
import castor.Context.Simple.global
import controller.actionlistenersimpl.{BoardActionListener, ConnectButtonActionListener, JoinGameButtonActionListener, ShowPlayersButtonActionListener, StartGameButtonActionListener}
import controller.animation.MoveAnimation
import status.GameStatus
import view.KalahaGuiCreator

import javax.swing.{JButton, JLabel, JTextField}

class KalahaClientConnection(gui: KalahaGuiCreator, game: GameStatus):
    var gameStatus: GameStatus = game
    var conn: WsClient = _
    var moveAnimation: MoveAnimation = new MoveAnimation(gui.boardPanel);

    var connectButton: JButton = gui.controlsPanel.connectionPanel.connectButton
    var showPlayersButton: JButton = gui.controlsPanel.playersPanel.showPlayersButton
    var nameSubmitButton: JButton = gui.controlsPanel.namePanel.nameSubmitButton
    var nameTextField: JTextField = gui.controlsPanel.namePanel.nameTextField
    var startGameButton: JButton = gui.controlsPanel.gamePanel.startGameButton

    connectButton.addActionListener(new ConnectButtonActionListener(this))
    showPlayersButton.addActionListener(new ShowPlayersButtonActionListener(this))
    nameSubmitButton.addActionListener(new JoinGameButtonActionListener(this, nameTextField))
    startGameButton.addActionListener(new StartGameButtonActionListener(this))

    def connectToServer(): Unit =
        if conn != null then throw new IllegalStateException("You are already connected!")
        conn = WsClient.connect("ws://localhost:8080/kalaha-websocket") {
            case Ws.Text("connected") =>
                gui.controlsPanel.connectionPanel.connectStatus.setText("Connected!")
            case Ws.Text("registered") =>
                println("Registered succesfully")
            case Ws.Text(s"players: ${firstPlayerName}, ${secondPlayerName}") =>
                gameStatus.playerNames(0) = firstPlayerName
                gameStatus.playerNames(1) = secondPlayerName
                gui.controlsPanel.playersPanel.firstPlayerLabel.setText(firstPlayerName)
                gui.controlsPanel.playersPanel.secondPlayerLabel.setText(secondPlayerName)
                val boardButtons: Array[JButton] = (if firstPlayerName == gameStatus.name then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
                for (i <- boardButtons.indices)
                    boardButtons(i).addActionListener(new BoardActionListener(this, i))
            case Ws.Text("game started") =>
                println("Game started!")
                val boardButtons: Array[JButton] = (if gameStatus.name == gameStatus.playerNames(0) then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
                for (i <- boardButtons.indices)
                    boardButtons(i).setEnabled(true)
            case Ws.Text(s"remaining $remaining") =>
                gui.controlsPanel.gamePanel.timeoutLabel.setText(s"Time: $remaining")
            case Ws.Text(s"${name} time is up") =>
                gui.controlsPanel.gamePanel.timeoutLabel.setText("You lose!")
            case Ws.Text(s"$name made move: $holeNumber") =>
                val playerNum = if name == gameStatus.playerNames(0) then 0 else 1
                moveAnimation.animate(playerNum, holeNumber.toInt)
                conn.send(Ws.Text("animationDone"))
            case Ws.Text(s) => println(s)
        }
        conn.send(Ws.Text("connect"))

    def joinGame(): Unit =
        if gameStatus.name == "" then throw new IllegalStateException("Enter your name first!")
        conn.send(Ws.Text(s"joinGame${gameStatus.name}"))

    def getPlayers(): Unit =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("showPlayers"))

    def startGame(): Unit =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("startGame"))

    def makeMove(holeNumber: Int) =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        if holeNumber < 0 || holeNumber > 5 then throw new IllegalArgumentException("Illegal hole number!")
        conn.send(Ws.Text(s"makeMove ${gameStatus.name}; $holeNumber"))

    def disconnect(): Unit =
        conn.send(Ws.Text("disconnect"))
