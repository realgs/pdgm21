import controller.KalahaController
import service.{KalahaService, TimerRunnable}
import cask.*

object Main extends MainRoutes:

    var controller: KalahaController = null

    @websocket("/kalaha-websocket/:x")
    def wsRoute(x: String): WebsocketResult =
        WsHandler { channel =>
            if controller == null then controller = new KalahaController(channel)
            WsActor {
                case Ws.Text("") => channel.send(Ws.Close())
                case Ws.Text("disconnect") =>
                    channel.send(Ws.Close())
                    controller.timerThread.interrupt()
                case Ws.Text("connect") =>
                    controller.onConnect
                case Ws.Text(s"joinGame$name") =>
                    controller.onJoinGame(name)
                case Ws.Text(s"makeMove$name $hole") =>
                    controller.onMakeMove(name, hole.toInt)
                case Ws.Text("showPlayers") =>
                    controller.onShowPlayers
                case Ws.Text("startGame") =>
                    controller.onStartGame
            }
        }

    initialize()