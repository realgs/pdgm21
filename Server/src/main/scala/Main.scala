import controller.KalahaController
import service.{KalahaService, TimerRunnable}
import cask.*

object Main extends MainRoutes:

    val controller = new KalahaController

    @websocket("/connect/:userName")
    def showUserProfile(userName: String): WebsocketResult =
        println(userName)
        WsHandler { channel =>
            WsActor {
                case Ws.Text("") => channel.send(Ws.Close())
                case Ws.Text(data) =>
                    if data.startsWith("connect") then
                        try
                            controller.registerPlayer(userName)
                            val players = controller.getPlayerNames
                            channel.send(Ws.Text("Registered " + userName + " players registered: " + players._1 + ", " + players._2))
                        catch
                            case e: Exception => channel.send(Ws.Text(e.getMessage))
                    else if data.startsWith("makeMove") then
                        try
                            val hole = data.substring(8).toInt
                            controller.makeMove(userName, hole)
                            controller.service.printBoard
                            channel.send(Ws.Text(userName + " " + data))
                        catch
                            case e: Exception => channel.send(Ws.Text(e.getMessage))
                    else if data.startsWith("showPlayers") then
                        val players = controller.getPlayerNames
                        channel.send(Ws.Text("Players registered: " + players._1 + ", " + players._2))
                    else if data.startsWith("startGame") then
                        try
                            controller.startGame
                            val players = controller.getPlayerNames
                            channel.send(Ws.Text("Game started with players: " + players._1 + ", " + players._2))
                            val thread = new Thread(new TimerRunnable(channel, controller))
                            thread.start
                        catch
                            case e: IllegalStateException => channel.send(Ws.Text(e.getMessage))
            }
        }

    initialize()