package pdgm21
import pdgm21.Player1Mancala
import pdgm21.Player2Mancala
import pdgm21.NumberOfHoles
import scala.language.postfixOps
import pdgm21.GameState
class Game():
  
  /*
  * 
  * representation is like indexes from 0 to 5 are player1 holes , indexes from 7 to 12 are player2 holese
  * indexes 6 and 13 are mancalas
  * value@index = number of stones in hole/mancala
  * 
  * */
  
  var gameBoard :Array[Int] = Array.fill(14)(0)


   def setBoard(newBoard:Array[Int]): Unit ={
     gameBoard = newBoard
   }


   def start(): Unit ={
     // setting number of stones of player 1
     for (i<- Player1StartHole to Player1EndHole)
       gameBoard(i) = 4
     gameBoard(Player1Mancala)=0

     // setting number of stones of player 2
     for(i<-Player2StartHole to Player2EndHole)
       gameBoard(i) = 4
     gameBoard(Player1Mancala) = 0
   }

   def printGameStatus(): Unit ={
     print("   ")
     for(i<- Player2EndHole to Player2StartHole by -1)
       print(gameBoard(i)+" ")

     println()
     println(gameBoard(Player2Mancala)+"                    "+gameBoard(Player1Mancala))
     print("   ")
     for (i<- Player1StartHole to Player1EndHole)
       print(gameBoard(i)+" ")
     println('\n')
   }

   def validateMove(chosenField:Int,playerId:Int):Boolean={
     if playerId ==1 then
       if(chosenField<Player1StartHole || chosenField>Player1EndHole || gameBoard(chosenField)==0) then
          return false
       else
         return true
         
     else
       if(chosenField<Player2StartHole || chosenField>Player2EndHole || gameBoard(chosenField)==0) then
        return false
       else
        return true
   }

   def isPlayersHole(hole:Int,playerId:Int): Boolean ={
     if(playerId==1)then
       if hole>=Player1StartHole && hole<=Player1EndHole then true
       else false
       
     else
       if hole>=Player2StartHole&&hole<=Player2EndHole then true
       else false
   }

   def addBonusStones(hole:Int, playerId:Int): Unit ={

     //12-hole = opposite hole

     if playerId==1 then
       gameBoard(Player1Mancala)+= gameBoard(12-hole)+1
       gameBoard(hole)=0
       gameBoard(12-hole)=0
     else
       gameBoard(Player2Mancala)+=gameBoard(12-hole)+1
       gameBoard(hole)=0
       gameBoard(12-hole)=0
   }

   def isLastHoleMancala(hole:Int,playerId:Int):Boolean={
     if playerId==1 && hole ==Player1Mancala then true
     else
       if playerId==2 &&hole ==Player2Mancala then true
       else false
   }


   def checkWin():Int={

     /*
  * 0 - no one won yet
  * 1 - player1 won
  * 2 - player2 won
  * 3 - player3 won
  * */



     var player1Ended = true
     var player2Ended = true

     //check if any player doesnt have any stones left
     for(i<- Player1StartHole to Player1EndHole)
       if(gameBoard(i)!=0) then
         player1Ended =  false

     for(j<-Player2StartHole to Player2EndHole)
       if(gameBoard(j)!=0) then
         player2Ended = false

     if(!player1Ended && !player2Ended) then
       return 0

     if(player1Ended) then
       for(i<- Player2StartHole to Player2EndHole)
         gameBoard(Player2Mancala) += gameBoard(i)
         gameBoard(i)=0

     else
       for(i<- Player1StartHole to Player1EndHole)
         gameBoard(Player1Mancala) += gameBoard(i)
         gameBoard(i)=0


     if gameBoard(Player1Mancala)>gameBoard(Player2Mancala) then
        1
     else if(gameBoard(Player1Mancala)<gameBoard(Player2Mancala))then
        2
     else
        3
   }



   def move(chosenFieldIndex:Int,playerId:Int):GameState={

     // oscilates between player1 and player2
     var nextPlayer=0
     if(playerId%2==0)then
        nextPlayer= 1
     else
        nextPlayer= 2


     if(validateMove(chosenFieldIndex,playerId)) then
      var numberOfStonesLeft= gameBoard(chosenFieldIndex)
      gameBoard(chosenFieldIndex)=0
      var currentIndex=  chosenFieldIndex+1
      var lastIndex= -1

      while (numberOfStonesLeft>0) {
        if(numberOfStonesLeft==1)
           lastIndex = currentIndex
           
           // skipping enemy mancala if its enemy one
        if((playerId==1 && currentIndex==Player2Mancala) || (playerId==2&&currentIndex==Player1Mancala)) then
          currentIndex = (currentIndex+1)%NumberOfHoles
        else
          //putting stone in hole 
          gameBoard(currentIndex)+=1
          currentIndex = (currentIndex+1)%NumberOfHoles
          numberOfStonesLeft-=1
      }


      if (gameBoard(lastIndex)==1 &&isPlayersHole(lastIndex,playerId)) then
        addBonusStones(lastIndex,playerId)
      else

      if(isLastHoleMancala(lastIndex,playerId)){
        nextPlayer = playerId
      }
      if(checkWin()== -1) then return new GameState(nextPlayer,0,gameBoard)
      else return new GameState(nextPlayer,checkWin(),gameBoard)
    else return new GameState(playerId,-1,gameBoard)
   }




