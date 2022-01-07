package controller

import cask.*
import cask.Logger.Console.globalLogger
import cask.util.WsClient
import castor.Context.Simple.global
import controller.actionlistenersimpl.{BoardActionListener, ConnectButtonActionListener, JoinGameButtonActionListener, ShowPlayersButtonActionListener, StartGameButtonActionListener}
import view.KalahaGuiCreator

import javax.swing.{JButton, JLabel, JTextField}

class KalahaClientConnection(gui: KalahaGuiCreator):
    var name = ""
    var conn: WsClient = _

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
        conn = WsClient.connect("ws://localhost:8080/kalaha-websocket/1") {
            case Ws.Text("connected") =>
                gui.controlsPanel.connectionPanel.connectStatus.setText("Connected!")
            case Ws.Text("registered") =>
                println("Registered succesfully")
            case Ws.Text(s"players: ${firstPlayerName}, ${secondPlayerName}") =>
                gui.controlsPanel.playersPanel.firstPlayerLabel.setText(firstPlayerName)
                gui.controlsPanel.playersPanel.secondPlayerLabel.setText(secondPlayerName)
                val boardButtons: Array[JButton] = (if firstPlayerName == name then gui.boardPanel.firstPlayerHoles else gui.boardPanel.secondPlayerHoles)
                for (i <- boardButtons.indices)
                    boardButtons(i).addActionListener(new BoardActionListener(this, i))
                    boardButtons(i).setEnabled(true)
            case Ws.Text("game started") =>
                println("Game started!")
            case Ws.Text(s"$name remaining $remaining") =>
                gui.controlsPanel.gamePanel.timeoutLabel.setText(s"Time: $remaining")
            case Ws.Text(s"${name} time is up") =>
                gui.controlsPanel.gamePanel.timeoutLabel.setText("You lose!")
            case Ws.Text(s"$name made move: $boardString") =>
                updateBoard(boardString)
            case Ws.Text(s) => println(s)
        }
        conn.send(Ws.Text("connect"))

    def joinGame(): Unit =
        if name == "" then throw new IllegalStateException("Enter your name first!")
        conn.send(Ws.Text(s"joinGame$name"))

    def getPlayers(): Unit =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("showPlayers"))

    def startGame(): Unit =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("startGame"))

    def makeMove(holeNumber: Int) =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        if holeNumber < 0 || holeNumber > 5 then throw new IllegalArgumentException("Illegal hole number!")
        conn.send(Ws.Text(s"makeMove $name; $holeNumber"))

    def updateBoard(boardString: String): Unit =
        boardString match
            case s"$f2, $e2, $d2, $c2, $b2, $a2 | $base2 | $base1 | $a1, $b1, $c1, $d1, $e1, $f1" =>
                val firstPlayerHoles: Array[JButton] = gui.boardPanel.firstPlayerHoles
                val bases: Array[JLabel] = gui.boardPanel.bases
                val secondPlayerHoles: Array[JButton] = gui.boardPanel.secondPlayerHoles
                firstPlayerHoles(0).setText(a1)
                firstPlayerHoles(1).setText(b1)
                firstPlayerHoles(2).setText(c1)
                firstPlayerHoles(3).setText(d1)
                firstPlayerHoles(4).setText(e1)
                firstPlayerHoles(5).setText(f1)
                bases(0).setText(base1)
                bases(1).setText(base2)
                secondPlayerHoles(0).setText(a2)
                secondPlayerHoles(1).setText(b2)
                secondPlayerHoles(2).setText(c2)
                secondPlayerHoles(3).setText(d2)
                secondPlayerHoles(4).setText(e2)
                secondPlayerHoles(5).setText(f2)
            case _ => throw new IllegalArgumentException("String format is not correct!")

    def disconnect(): Unit =
        conn.send(Ws.Text("disconnect"))
