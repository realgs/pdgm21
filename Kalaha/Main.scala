package Kalaha

import Kalaha.Gameboard.Gameboard
import Kalaha.Server.Server

object Main{
  def test() ={
//    var gameboard = new Gameboard
//    gameboard.prepareBoard()
//
//    println(gameboard.houses(0).toList )
//    println(gameboard.houses(1).toList )


    // Sprawdź czy jak 0

//    println(gameboard.move(0,0))
//    println(gameboard.houses(0).toList )
//    println(gameboard.houses(1).toList )
//    println(gameboard.move(3,0))
//    println(gameboard.houses(0).toList )
//    println(gameboard.houses(1).toList )
//    println(gameboard.move(2,0))
//    println(gameboard.houses(0).toList )
//    println(gameboard.houses(1).toList )


     var server = new Server
     server.game()


  }

  def main(args: Array[String]): Unit = {

    test()

  }

}
