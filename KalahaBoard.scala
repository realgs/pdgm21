package gameUtensils
import scala.annotation.tailrec
import scala.util.Random

class KalahaBoard(private var activePlayer: Int)
{
  private val generateRandom = Random

  private val INITIAL_NUMBER_OF_STONES = 4
  // number of pits excluding base
  private val NUMBER_OF_PITS_FOR_PLAYER = 6
  private val NUMBER_OF_PITS = 2 * NUMBER_OF_PITS_FOR_PLAYER + 2
  // characteristic indexes of players' pits
  private val PLAYER_1_ID = 1
  private val PLAYER_2_ID = 2
  private val PLAYER_1_START_INDEX = 0
  private val PLAYER_1_BASE_INDEX = NUMBER_OF_PITS_FOR_PLAYER
  private val PLAYER_2_START_INDEX = PLAYER_1_BASE_INDEX + 1
  private val PLAYER_2_BASE_INDEX = PLAYER_2_START_INDEX + NUMBER_OF_PITS_FOR_PLAYER
  // prepare pits to game
  private var pits: Array[Int] = Array.fill(NUMBER_OF_PITS)(INITIAL_NUMBER_OF_STONES)
  pits(PLAYER_1_BASE_INDEX) = 0
  pits(PLAYER_2_BASE_INDEX) = 0

  def resultForPlayer(playerID: Int): Int =
    if playerID == 1 then pits(PLAYER_1_BASE_INDEX) - pits(PLAYER_2_BASE_INDEX)
    else  pits(PLAYER_2_BASE_INDEX) - pits(PLAYER_1_BASE_INDEX)

  def getNumberOfStonesInPit(pitIndex: Int): Int =
    pits(pitIndex)

  def getActivePlayer(): Int =
    activePlayer

  def copy(): KalahaBoard =
    val board = new KalahaBoard(activePlayer)
    board.pits = pits.clone()
    board

  def indexLegend(): String =
    s"\n         (  12  ) (  11  ) (  10  ) (  9  ) (  8  ) (  7  )          <- Player's 2 pits\n" +
      s"(           )                                         (           )\n" +
      s"      13                                                    6\n" +
      s"(           )                                         (           )\n" +
      s"          (  0  ) (  1  ) (  2  ) (  3  ) (  4  ) (  5  )            <- Player's 1 pits\n"

  def showBoard(): String =
    s"\n          (  ${pits(12)}  ) (  ${pits(11)}  ) (  ${pits(10)}  ) (  ${pits(9)}  ) (  ${pits(8)}  ) (  ${pits(7)}  )\n" +
      s"(           )                                         (           )\n" +
      s"      ${pits(13)}                                                     ${pits(6)}\n" +
      s"(           )                                         (           )\n" +
      s"          (  ${pits(0)}  ) (  ${pits(1)}  ) (  ${pits(2)}  ) (  ${pits(3)}  ) (  ${pits(4)}  ) (  ${pits(5)}  )\n"

  def showScore(): String =
    "Player's 1 final score: " +pits(PLAYER_1_BASE_INDEX) +
      "\nPlayer's 2 final score: " +pits(PLAYER_2_BASE_INDEX)

  def showWinningPLayer(): String =
    if pits(PLAYER_1_BASE_INDEX) == pits(PLAYER_2_BASE_INDEX) then "Remis!"
    else if pits(PLAYER_1_BASE_INDEX) > pits(PLAYER_2_BASE_INDEX) then "Player 1 won!"
    else "Player 2 won!"

  def switchActivePlayer(): Unit =
    if activePlayer == PLAYER_1_ID then activePlayer = PLAYER_2_ID
    else activePlayer = PLAYER_1_ID

  def onlyOneOption(): Int =
    var counter = 0
    var moveOption = -1
    val offset = if activePlayer == PLAYER_1_ID then 0 else 7
    for(i<- 0 to 5)
      if pits(i + offset) != 0 then {counter += 1; moveOption = i + offset} else ()
    if counter == 1 then moveOption else -1

  def isMoveCorrect(pit: Int): Boolean =
    (activePlayer == PLAYER_1_ID && pit >= PLAYER_1_START_INDEX && pit < PLAYER_1_BASE_INDEX && pits(pit) != 0) ||
      (activePlayer == PLAYER_2_ID && pit >= PLAYER_2_START_INDEX && pit < PLAYER_2_BASE_INDEX && pits(pit) != 0)

  def didPlayer1End(): Boolean =
    activePlayer == 1 && pits.slice(PLAYER_1_START_INDEX, PLAYER_1_BASE_INDEX).sum == 0

  def didPlayer2End(): Boolean =
    activePlayer == 2 && pits.slice(PLAYER_2_START_INDEX, PLAYER_2_BASE_INDEX).sum == 0

  def isGameFinished(): Boolean =
    if didPlayer1End() then {finishedGameActionsPlayer2(); true}
    else if didPlayer2End() then {finishedGameActionsPlayer1(); true}
    else false

  def finishedGameActionsPlayer1(): Unit =
    pits(PLAYER_1_BASE_INDEX) += pits.slice(PLAYER_1_START_INDEX, PLAYER_1_BASE_INDEX).sum

  def finishedGameActionsPlayer2(): Unit =
    pits(PLAYER_2_BASE_INDEX) += pits.slice(PLAYER_2_START_INDEX, PLAYER_2_BASE_INDEX).sum

  def endOfMoveInPlayersEmptyPit(endPit: Int): Unit =
    val stolenStonesIndex = PLAYER_2_BASE_INDEX - 1 - endPit
    if pits(stolenStonesIndex) == 0 then () else
      if (activePlayer == PLAYER_1_ID) then
        pits(PLAYER_1_BASE_INDEX) = pits(PLAYER_1_BASE_INDEX) + pits(stolenStonesIndex) + 1
      else
        pits(PLAYER_2_BASE_INDEX) = pits(PLAYER_2_BASE_INDEX) + pits(stolenStonesIndex) + 1
      pits(stolenStonesIndex) = 0
      pits(endPit) = 0

  def moveStones(pit: Int, stones: Int): Boolean =
    if stones > 0 then
      if activePlayer == PLAYER_1_ID then
        if pit == PLAYER_2_BASE_INDEX then
          moveStones((pit + 1) % NUMBER_OF_PITS, stones)
        else
          pits(pit) += 1
          moveStones((pit + 1) % NUMBER_OF_PITS, stones - 1)
      else
        if pit == PLAYER_1_BASE_INDEX then
          moveStones((pit + 1) % NUMBER_OF_PITS, stones)
        else
          pits(pit) += 1
          moveStones((pit + 1) % NUMBER_OF_PITS, stones - 1)
    else
      val lastPit = if pit != 0 then pit - 1 else PLAYER_2_BASE_INDEX
      if activePlayer == 1 then
        if lastPit == PLAYER_1_BASE_INDEX then false
        else if lastPit >= PLAYER_1_START_INDEX && lastPit < PLAYER_1_BASE_INDEX && pits(lastPit) == 1 then
          endOfMoveInPlayersEmptyPit(lastPit)
          true
        else true
      else
        if lastPit == PLAYER_2_BASE_INDEX then false
        else if lastPit >= PLAYER_2_START_INDEX && lastPit < PLAYER_2_BASE_INDEX && pits(lastPit) == 1 then
          endOfMoveInPlayersEmptyPit(lastPit)
          true
        else true

  def parcelOutStones(pitChosenByPlayer: Int): Boolean =
    val rememberHowManyStonesInPit = pits(pitChosenByPlayer)
    pits(pitChosenByPlayer) = 0
    moveStones(pitChosenByPlayer + 1, rememberHowManyStonesInPit)

  def performMoveOfOnePlayer(pitChosenByPlayer: Int): Boolean =
    if parcelOutStones(pitChosenByPlayer) then switchActivePlayer() else ()
    if isGameFinished() then true else false

}
