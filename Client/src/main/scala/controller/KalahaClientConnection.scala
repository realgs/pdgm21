package controller

import cask.*
import cask.Logger.Console.globalLogger
import cask.util.WsClient
import castor.Context.Simple.global

class KalahaClientConnection:
    var name = ""
    var conn: WsClient = _

    def connectToServer =
        if conn != null then throw new IllegalStateException("You are already connected!")
        conn = WsClient.connect("ws://localhost:8080/kalaha-websocket/1") {
            case Ws.Text(s) =>
                if s.startsWith(s"$name, you lose") then
                    name = ""
                    conn.send(Ws.Close())
                println(s)
        }
        conn.send(Ws.Text("connect"))

    def joinGame =
        if name == "" then throw new IllegalStateException("Enter your name first!")
        conn.send(Ws.Text(s"joinGame$name"))

    def getPlayers =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("showPlayers"))

    def startGame =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("startGame"))

    def makeMove(holeNumber: Int) =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        if holeNumber < 0 || holeNumber > 5 then throw new IllegalArgumentException("Illegal hole number!")
        conn.send(Ws.Text(s"makeMove$name $holeNumber"))

    def disconnect =
        conn.send(Ws.Text("disconnect"))
