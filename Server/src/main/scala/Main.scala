import controller.KalahaController
import service.{KalahaService, TimerRunnable}
import cask.*

object Main extends MainRoutes:

    var controller: KalahaController = null

    @websocket("/connect/:userName")
    def showUserProfile(userName: String): WebsocketResult =
        println(userName)
        WsHandler { channel =>
            WsActor {
                case Ws.Text("") => channel.send(Ws.Close())
                case Ws.Text(data) =>
                    if controller == null then controller = new KalahaController(channel)
                    if data.startsWith("connect") then
                        controller.onConnect(userName)
                    else if data.startsWith("makeMove") then
                        val hole = data.substring(8).toInt
                        controller.onMakeMove(userName, hole)
                    else if data.startsWith("showPlayers") then
                        controller.onShowPlayers
                    else if data.startsWith("startGame") then
                        controller.onStartGame
            }
        }

    initialize()