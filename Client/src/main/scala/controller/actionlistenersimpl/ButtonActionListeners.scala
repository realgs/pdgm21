package controller.actionlistenersimpl

import controller.KalahaController
import controller.connection.KalahaClientConnection

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JTextField

class ConnectButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        println("connect pressed")
        conn.connectToServer()
        
class ShowPlayersButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        println("get players pressed")
        conn.commandGetPlayers()

class JoinGameButtonActionListener(conn: KalahaClientConnection, controller: KalahaController) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        println("join game pressed")
        controller.gameStatus.name = controller.nameTextField.getText()
        conn.commandJoinGame()
        conn.commandGetPlayers()
        
class StartGameButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        println("start game pressed")
        conn.commandStartGame()

class BoardActionListener(conn: KalahaClientConnection, id: Int) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        println("board pressed " + id)
        conn.commandMakeMove(id)
