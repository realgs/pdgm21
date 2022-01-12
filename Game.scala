package pdgm21
import pdgm21.Player1BaseIndex
import pdgm21.Player2BaseIndex
import pdgm21.NumberOfHoles
import scala.language.postfixOps
import pdgm21.GameState
class Game():
   private val gameBoard :Array[Int] = Array.fill(14)(0)


   def start(): Unit ={
     // setting number of stones of player 1
     for (i<- 0 to 5)
       gameBoard(i) = 4
     gameBoard(Player1BaseIndex)=0

     // setting number of stones of player 2
     for(i<-7 to 12)
       gameBoard(i) = 4
     gameBoard(Player1BaseIndex) = 0
   }

   def printGameStatus(): Unit ={
     print("   ")
     for(i<- 12 to 7 by -1)
       print(gameBoard(i)+" ")

     println()
     println(gameBoard(Player2BaseIndex)+"                    "+gameBoard(Player1BaseIndex))
     print("   ")
     for (i<- 0 to 5)
       print(gameBoard(i)+" ")
     println('\n')
   }

   def validateMove(chosenField:Int,playerId:Int):Boolean={
     if playerId ==1 then
       if(chosenField<0 || chosenField>5 || gameBoard(chosenField)==0) then
          return false
       else
         return true
         
     else
       if(chosenField<7 || chosenField>12 || gameBoard(chosenField)==0) then
        return false
       else
        return true
   }

   def isPlayersHole(hole:Int,playerId:Int): Boolean ={
     if(playerId==1)then
       if hole>0&&hole<6 then true
       else false
       
     else
       if hole>6&&hole<13 then true
       else false
   }

   def addBonusStones(hole:Int, playerId:Int): Unit ={
     if playerId==1 then
       gameBoard(Player1BaseIndex)+= gameBoard(12-hole)+1
       gameBoard(hole)=0
       gameBoard(12-hole)=0
     else
       gameBoard(Player2BaseIndex)+=gameBoard(12-hole)+1
       gameBoard(hole)=0
       gameBoard(12-hole)=0
   }

   def isLastHoleMancala(hole:Int,playerId:Int):Boolean={
     if playerId==1 && hole ==Player1BaseIndex then true
     else
       if playerId==2 &&hole ==Player2BaseIndex then true
       else false
   }


   // returns 0 if no one won, 1 if player1 , 2 if player 2,3 if draw
   def checkWin():Int={
     var player1Ended = true
     var player2Ended = true

     //check if any player doesnt have any stones left
     for(i<- 0 to 5)
       if(gameBoard(i)!=0) then
         player1Ended =  false

     for(j<-7 to 12)
       if(gameBoard(j)!=0) then
         player2Ended = false

     if(!player1Ended && !player2Ended) then
       return 0

     if(player1Ended) then
       for(i<- 7 to 12)
         gameBoard(Player2BaseIndex) += gameBoard(i)
         gameBoard(i)=0

     else
       for(i<- 0 to 5)
         gameBoard(Player1BaseIndex) += gameBoard(i)
         gameBoard(i)=0


     if gameBoard(Player1BaseIndex)>gameBoard(Player2BaseIndex) then
       return 1
     else if(gameBoard(Player1BaseIndex)<gameBoard(Player2BaseIndex))then
       return 2
     else
       return 3
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
      var currentIndex=  chosenFieldIndex
      var lastIndex= -1

      while (numberOfStonesLeft>0) {
        
        if(numberOfStonesLeft==1)
           lastIndex = currentIndex
           
           // skipping enemy mancala if
        if((playerId==1 && currentIndex==Player2BaseIndex) || (playerId==2&&currentIndex==Player1BaseIndex)) then
          currentIndex = (currentIndex+1)%NumberOfHoles
        else
          //putting stone in hole 
          gameBoard(currentIndex)+=1
          currentIndex = (currentIndex+1)%NumberOfHoles
          numberOfStonesLeft-=1
      }
     // println(lastIndex+" " +gameBoard(lastIndex))


      if (gameBoard(lastIndex)==1 &&isPlayersHole(lastIndex,playerId)) then
        addBonusStones(lastIndex,playerId)
       // println("added")
      else
     //   println("notAdded")

      if(isLastHoleMancala(lastIndex,playerId)){
        nextPlayer = playerId
      }
      if(checkWin()== -1) then return new GameState(nextPlayer,0,gameBoard)
      else return new GameState(nextPlayer,checkWin(),gameBoard)
    else return new GameState(playerId,-1,gameBoard)
   }




