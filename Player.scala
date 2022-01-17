package Kalah
import java.util.Scanner

class Player(s_name:String, index:Int) extends Opponent(s_name, index) {

  override
  def move(board: Board): Int = {
    consoleMove
  }

  def consoleMove:Int={
    val sc=new Scanner(System.in)
    var chosenHouse = 0
    while (chosenHouse == 0 || chosenHouse == 7 || chosenHouse > 13 || chosenHouse < 0){
      try
        println("Choose house")
        chosenHouse=sc.nextInt()
    }
    chosenHouse
  }

}
