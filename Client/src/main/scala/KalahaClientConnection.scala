import cask._
import cask.util.WsClient
import cask.Logger.Console.globalLogger
import castor.Context.Simple.global

class KalahaClientConnection:

    var name = ""
    var conn: WsClient = _

    def connectToServer =
        if name == "" then throw new IllegalStateException("Enter your name first!")
        if conn != null then throw new IllegalStateException("You are already connected!")
        conn = WsClient.connect(s"ws://localhost:8080/connect/$name") {
            case Ws.Text(s) =>
                if s.startsWith(s"$name, you lose") then
                    name = ""
                    conn.send(Ws.Close())
                println(s)
        }
        conn.send(Ws.Text(s"connect$name"))

    def getPlayers =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("showPlayers"))

    def startGame =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        conn.send(Ws.Text("startGame"))

    def makeMove(holeNumber: Int) =
        if conn == null then throw new IllegalStateException("Establish your connection first!")
        if holeNumber < 0 || holeNumber > 5 then throw new IllegalArgumentException("Illegal hole number!")
        conn.send(Ws.Text(s"makeMove$holeNumber"))

    def disconnect =
        conn.send(Ws.Text("disconnect"))
