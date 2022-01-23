package boards

import players.Player.Player

object Board {
  class Board{
  /*class Board(var Player1Row: Array[Int] = Array(0,0,0,0,0,0), var Player1House: Int = 0, var Player2Row: Array[Int] = Array(0,0,0,0,0,0),
              var Player2House: Int = 0, val ALL_STONES:Int = 72, var Player1: Player, var Player2: Player) {*/
    var Player1Row: Array[Int] = Array(0,0,0,0,0,0)
    var Player1House: Int = 0
    var Player2Row: Array[Int] = Array(0,0,0,0,0,0)
    var Player2House: Int = 0
    val ALL_STONES = 72
    //var Player1: Player
    //var Player2: Player

    def setBoard(): Unit ={
      var temp : Int = ALL_STONES/12
      Player1Row = Array(temp,temp,temp,temp,temp,temp)
      Player1House = 0
      Player2Row = Array(temp,temp,temp,temp,temp,temp)
      Player2House = 0
      printBoard()
    }

    def checkSumOfStones(): Int ={
      Player1Row(0)+ Player1Row(1)+ Player1Row(2)+ Player1Row(3)+ Player1Row(4) + Player1Row(5) +  Player2Row(0)+ Player2Row(1)+ Player2Row(2)+ Player2Row(3)+ Player2Row(4) + Player2Row(5) +Player2House+ Player1House
    }

    def someTestforPrint(): Unit ={
      Player1Row(2)=12
      Player2Row(3)=16
      Player1House=10
    }

    def printBoard(): Unit ={
     println(s"\n    \u001B[36m${Player1Row(0)}    ${Player1Row(1)}    ${Player1Row(2)}    ${Player1Row(3)}    ${Player1Row(4)}    ${Player1Row(5)}"+
     s"\n\u001B[1m${Player1House}                                \u001B[32m${Player2House}\u001B[0m  "+
       s"\n    \u001B[32m${Player2Row(0)}    ${Player2Row(1)}    ${Player2Row(2)}    ${Player2Row(3)}    ${Player2Row(4)}    ${Player2Row(5)}\u001B[0m");

    }

    def moveInFirstRow(stones:Int, player:Int, start:Int):Int={
      if player==1 then{
        if (start == 0 && Player1Row(0)==0) stones
        else{
          var i =start
          var newSt = stones
          while(newSt!=0 && i>=0){
            if Player1Row(i) == 0 then {
              Player1House = Player1House + Player2Row(i)
              Player2Row(i) = 0;
            }
            Player1Row(i) += 1
            i -= 1
            newSt -= 1
          }
          newSt
        }
      }
      else{
        var i =start
        var newSt = stones
        while(newSt!=0 && i>=0){
          Player1Row(i) += 1
          i -= 1
          newSt -= 1
        }
        newSt
      }
    }

    def moveInSecRow(stones: Int, player: Int, start:Int):Int={
      if player==1 then{
        var i =start
        var newSt = stones
        while(newSt!=0 && i<Player2Row.size){
          Player2Row(i) += 1
          i += 1
          newSt -= 1
        }
        newSt
      }
      else{
        println("I'M HERE!")
        if (start==Player2Row.size-1 && Player2Row(Player2Row.size-1)==0) stones
        else{
          var i =start
          var newSt = stones
          while(newSt!=0 && i<Player2Row.size){
            if Player2Row(i)==0 then{
              Player2House = Player2House + Player1Row(i)
              Player1Row(i) = 0;
            }
            println(i+": "+Player2Row(i)+"...stones: "+newSt)
            Player2Row(i) += 1
            i+=1
            newSt -= 1
          }
          newSt
        }
      }
    }


    def movingPieces(index:Int, row:Int): Boolean ={
      if(row == 1) then {
        var stones = Player1Row(index)
        Player1Row(index) = 0
        var i = index - 1
        if i == -1 then i = 0
        while (stones != 0) {
          stones = moveInFirstRow(stones, 1, i)
          //println("kamienie"+stones)
          if (stones == 1) {
            Player1House += 1
            return true
          }

          else if (stones > 1) {
            Player1House += 1
            stones -= 1
            i = 0
          }
          stones = moveInSecRow(stones, 1, i)
          //println("kamieniePoSec"+stones)
        }
        false
      }
      else {
        var stones = Player2Row(index)
        Player2Row(index) = 0
        var i = index + 1
        if i == Player2Row.size then i = Player2Row.size-1
        while(stones!=0){
          if i == Player2Row.size-1 then println("Somthings wrong with stones, before "+ stones)
          stones = moveInSecRow(stones, 2, i)
          if i == Player2Row.size-1 then println("Somthings wrong with stones "+ stones)

          if (stones == 1) {
            Player2House += 1
            return true
          }

          else if(stones>1){
            Player2House +=1
            stones-=1
            i = Player1Row.size-1
          }

          stones = moveInFirstRow(stones, 2, i)

        }

        false

      }
    }

    def gameOver(): Unit ={
      var winner =0
      if Player1House>Player2House then winner =1
      else if Player2House>Player1House then winner =2
      else println(s"Draw!")
      if winner!=0 then println(s"Player$winner won!")
    }

    def checkRow(row: Array[Int]):Boolean={
      var i =0
      var j=0
      while(i<row.size){
        if row(i)==0 then j+=1
        i+=1
      }
      if j==6 then true
      else false
    }

    def endOfTheGame(): Boolean ={
      if(Player1House>ALL_STONES/2 || Player2House>ALL_STONES/2) then true
      else if(checkRow(Player1Row)||checkRow(Player2Row)) then true
      else false
    }
  }

}
