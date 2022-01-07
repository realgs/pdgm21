package controller.actionlistenersimpl

import controller.KalahaClientConnection

import java.awt.event.{ActionEvent, ActionListener}

class BoardActionListener(conn: KalahaClientConnection, id: Int) extends ActionListener:
    override def actionPerformed(e: ActionEvent): Unit =
        conn.makeMove(id)
