package controller.actionlistenersimpl

import cask.util.{Ws, WsClient}
import controller.KalahaClientConnection

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.JTextField

class ConnectButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.connectToServer()
        
class ShowPlayersButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.getPlayers()

class JoinGameButtonActionListener(conn: KalahaClientConnection, nameTextField: JTextField) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.name = nameTextField.getText()
        conn.joinGame()
        conn.getPlayers()
        
class StartGameButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.startGame()