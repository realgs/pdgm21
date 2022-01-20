class Human() extends Player{
  private var result = -1
  private var currentMapState: Array[Array[Int]] = Array.fill(2)(Array.fill(7)(0))
  private var player: Int = 0
  private var wantPlay: Int = 0

  def Start(): Unit ={
    println("Welcome!!!" +
      "\n1. Player vs Player" +
      "\n2. Player vs Computer" +
      "\n3. Computer vs Computer" +
      "\n4. End")
    var line = "";
    while(line == ""){
      line = Console.in.readLine();
      line match
        case "1" => wantPlay = 1
        case "2" => wantPlay = 2
        case "3" => wantPlay = 3
        case "4" => wantPlay = 0
        case _ =>
    }

  }

  def GetWantPlay(): Int = wantPlay

  override def ReturnMove(): Int =
    if result == -1
    then
      CheckAvailableMoves(currentMapState,player).head
    else
      result
  @Override
  override def Possibilities(startMapState: Array[Array[Int]],playerNum: Int): Unit = {
    player = playerNum
    result = -1
    currentMapState = startMapState
    print("Podaj Liczbę: ")
    var line  = Console.in.readLine();
    if line != ""
    then
      while(!CheckAvailableMoves(currentMapState,player).contains(line.charAt(0).asDigit)){
        print("Podaj Liczbę: ")
        line = Console.in.readLine();
        println("")
      }
      result = line.charAt(0).asDigit
  }

  override def searchingNeed(): Boolean =
    if result == -1  then true else false

}
