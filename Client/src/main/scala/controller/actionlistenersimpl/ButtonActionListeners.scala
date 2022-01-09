package controller.actionlistenersimpl

import controller.KalahaController
import controller.connection.KalahaClientConnection

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JTextField}

class ConnectButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.connectToServer()
        
class ShowPlayersButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.commandGetPlayers()

class JoinGameButtonActionListener(conn: KalahaClientConnection, controller: KalahaController) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        controller.gameStatus.name = controller.nameTextField.getText()
        conn.commandJoinGame()
        conn.commandGetPlayers()
        
class StartGameButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.commandStartGame()

class BoardActionListener(conn: KalahaClientConnection, controller: KalahaController, id: Int) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        val yourBoardButtons: Array[JButton] =
            (if controller.gameStatus.playerNames(0) == controller.gameStatus.name then controller.gui.boardPanel.firstPlayerHoles else controller.gui.boardPanel.secondPlayerHoles)
        for (button <- yourBoardButtons)
            button.setEnabled(false)
        conn.commandMakeMove(id)
