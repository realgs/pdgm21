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
     println()
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

   def checkWin():Int={
     var notChecked1 = true
     var notChecked2 = true
     for(i<- 0 to 5)
       if(gameBoard(i)!=0) then
         notChecked1 =  false

     for(j<-7 to 12)
       if(gameBoard(j)!=0) then
         notChecked2 = false

     if(!notChecked1 && !notChecked2) then
       return 0
     
     if gameBoard(Player1BaseIndex)>gameBoard(Player2BaseIndex) then
       1
     else
       2
   }


  // TODO think about changing boolean to int and specyfing exit codes or throwing exceptions
  //  (exception for winning seems odd)
   def move(chosenFieldIndex:Int,playerId:Int):GameState={
     var nextPlayer=0
     if(playerId%2==0)then
        nextPlayer= 1
     else
        nextPlayer= 2

     if(validateMove(chosenFieldIndex,playerId)) then
      var numberOfStones= gameBoard(chosenFieldIndex)
      gameBoard(chosenFieldIndex)=0
      var currentIndex=  chosenFieldIndex+1
      var lastIndex= -1

      while (numberOfStones>0) {
        if(numberOfStones==1)
           lastIndex = currentIndex
        if((playerId==1 && currentIndex==13) || (playerId==2&&currentIndex==6)) then
          currentIndex = (currentIndex+1)%NumberOfHoles
        else
          gameBoard(currentIndex)+=1
          currentIndex = (currentIndex+1)%NumberOfHoles
          numberOfStones-=1
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
    else return new GameState(nextPlayer,-1,gameBoard)
   }




