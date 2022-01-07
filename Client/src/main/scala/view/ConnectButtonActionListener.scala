package view

import java.awt.event.{ActionEvent, ActionListener}
import cask.util.{Ws, WsClient}
import controller.KalahaClientConnection

class ConnectButtonActionListener(conn: KalahaClientConnection) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.connectToServer
