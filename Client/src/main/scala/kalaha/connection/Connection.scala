package kalaha.connection

import kalaha.controller.Controller
import kalaha.view.animation.MoveAnimation
import kalaha.status.GameStatus
import kalaha.view.GuiCreator

import java.io.{BufferedReader, InputStreamReader, PrintWriter}
import java.net.{InetAddress, Socket}
import javax.swing.{JButton, JLabel, JTextField}

class Connection(controller: Controller):
    var socket: Socket = null
    var reader: BufferedReader = null
    var writer: PrintWriter = null

    def connectToServer: Unit =
        socket = new Socket("127.0.0.1", 6666)
        reader = new BufferedReader(new InputStreamReader(socket.getInputStream()))
        writer = new PrintWriter(socket.getOutputStream(), true)
        new Thread(new ReaderRunnable(reader, controller)).start()

    def commandJoinGame: Unit =
        writer.println(s"joinGame${controller.gameStatus.name}")

    def commandGetPlayers: Unit =
        writer.println("showPlayers")

    def commandStartGame: Unit =
        writer.println("startGame")

    def commandMakeMove(holeNumber: Int) =
        require(holeNumber >= 0 && holeNumber < 6, "Illegal hole number!")
        writer.println(s"makeMove ${controller.gameStatus.name}; $holeNumber")

    def commandDisconnect: Unit =
        writer.println("disconnect")

    def commandAnimationDone: Unit =
        writer.println("animationDone")