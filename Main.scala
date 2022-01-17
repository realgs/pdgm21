package Server
import Game.Board
import Client.Computer
import Client.Client

object Main :
  def main(ars : Array[String]) : Unit =
    var server = new Server
    server.startGame
end Main
