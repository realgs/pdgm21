import java.util.concurrent.atomic.AtomicInteger
import castor.Context.Simple.global

import cask.Logger.Console.globalLogger
import cask._

object Main:
    def main(args: Array[String]) =
        val ws = WsClient.connect("ws://localhost:8080/connect/haoyi"){
            case Ws.Text(s) => println(s)
        }

        val ws2 = WsClient.connect("ws://localhost:8080/connect/filip"){
            case Ws.Text(s) => println(s)
        }

        ws.send(Ws.Text("connecthaoyi"))
        Thread.sleep(1000)
        ws2.send(Ws.Text("connectfilip"))
        Thread.sleep(1000)
        ws.send(Ws.Text("startGame"))
        Thread.sleep(1000)
        ws.send(Ws.Text("makeMove0"))
        Thread.sleep(1000)
        ws.send(Ws.Text("makeMove1"))
        Thread.sleep(1000)
        ws2.send(Ws.Text("makeMove0"))
        Thread.sleep(1000)
        ws.send(Ws.Text(""))
        Thread.sleep(1000)
        ws2.send(Ws.Text(""))
        Thread.sleep(100)
