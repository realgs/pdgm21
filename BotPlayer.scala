import java.util.Random

class BotPlayer
{
//  private var p1Bowl1 = Bowl(6, 1)
//  private var p1Bowl2 = Bowl(6, 2)
//  private var p1Bowl3 = Bowl(6, 3)
//  private var p1Bowl4 = Bowl(6, 4)
//  private var p1Bowl5 = Bowl(6, 5)
//  private var p1Bowl6 = Bowl(6, 6)
//  private var p1Score = Bowl(0, 7)
//  private var p1Bowls = List(p1Bowl1, p1Bowl2, p1Bowl3, p1Bowl4, p1Bowl5, p1Bowl6, p1Score)
//
//  private var p2Bowl1 = Bowl(6, 1)
//  private var p2Bowl2 = Bowl(6, 2)
//  private var p2Bowl3 = Bowl(6, 3)
//  private var p2Bowl4 = Bowl(6, 4)
//  private var p2Bowl5 = Bowl(6, 5)
//  private var p2Bowl6 = Bowl(6, 6)
//  private var p2Score = Bowl(0, 7)
//  private var p2Bowls = List(p2Bowl1, p2Bowl2, p2Bowl3, p2Bowl4, p2Bowl5, p2Bowl6, p2Score)

  def play(): Int =
    val result = scala.util.Random
    result.nextInt(6) + 1

//  class DecisionTree(var points: Int, val listOfChildren: List[DecisionTree])
//
//  private def makeDecisionTree(depth: Int): DecisionTree =
//    if depth > 0 then
//      DecisionTree(-73, List(makeDecisionTree(depth - 1), makeDecisionTree(depth - 1), makeDecisionTree(depth - 1), makeDecisionTree(depth - 1), makeDecisionTree(depth - 1), makeDecisionTree(depth - 1)))
//    else
//      DecisionTree(-73, List())
//
//  var tree = makeDecisionTree(7)
//
//  //    def play(): Int =
//  //      tree = makeDecisionTree(5)
//  //      findBestMove(tree, 5, -73, 73, true, List())
//
//  def findBestMove(tree: DecisionTree, depth: Int, alpha: Int, beta: Int, playerMove: Boolean, steps: List[Int]): Int =
//    var maxAlpha = alpha
//    var minBeta = beta
//    var childrenList = tree.listOfChildren
//
//    if depth == 0 then
//      tree.points = evaluatePosition(steps)
//      return tree.points //pozmieniać!!!
//
//    if playerMove then
//      var maxFindPoints = -73
//      var i = childrenList.size
//      while i > 0 do
//        var findPoints = findBestMove(childrenList.head, depth - 1, alpha, beta, false, i::steps)//zastanowić się co z podwójnym ruchem
//        maxFindPoints = Math.max(maxFindPoints, findPoints)
//        maxAlpha = Math.max(maxAlpha, findPoints)
//        i = i - 1
//        if minBeta <= maxAlpha then
//          i = 0
//      return maxFindPoints
//    else
//      var minFindPoints = 73
//
//      var i = childrenList.size
//      while i > 0 do
//        var findPoints = findBestMove(childrenList.head, depth - 1, alpha, beta, true, i::steps)//zastanowić się co z podwójnym ruchem
//        minFindPoints = Math.min(minFindPoints, findPoints)
//        minBeta = Math.min(minBeta, findPoints)
//        i = i - 1
//        if minBeta <= maxAlpha then
//          i = 0
//      return minFindPoints
//
//  def evaluatePosition(steps: List[Int]): Int =
//    def move(playerBowls: List[Bowl], oponentBowls: List[Bowl], playerScore: Bowl, numberOfBowls: Integer): Boolean =
//      var FreeMove = false
//      var list = playerBowls
//      var oponentList = oponentBowls
//      var i = list.head.value
//      var j = numberOfBowls
//
//      while  j >= 0 do
//        i = list.head.value
//        if j == 0 then
//          list.head.value = 0
//        list = list.tail
//        j = j - 1;
//
//
//      while i > 0 do
//        if !list.isEmpty then
//          list.head.increase()
//          if list.head.numberOfBowl == 7 && i == 1 then // Ostatni kamyk wpada do naszej bazy
//            FreeMove = true
//            return FreeMove
//
//          if i == 1 && list.head.value == 1 then // Zbiajnie kamyków u przeciwnika
//            list.head.decrease()
//            playerScore.increase()
//            var BowlNumber = 6 - list.head.numberOfBowl
//            oponentList = oponentBowls
//
//            while  BowlNumber >= 0 do
//              if BowlNumber == 0 then
//                playerScore.value = playerScore.value + oponentList.head.value
//                oponentList.head.value = 0
//
//              BowlNumber = BowlNumber - 1
//              oponentList = oponentList.tail
//
//          list = list.tail
//
//        else
//          if oponentList.head.numberOfBowl != 7 then
//            oponentList.head.increase()
//            oponentList = oponentList.tail
//          else
//            list = playerBowls
//            oponentList = oponentBowls
//            i = i + 1
//        i = i - 1
//      FreeMove
//    var leftSteps = steps
//    var player = true
//    while !leftSteps.isEmpty do
//      if player then
//        if !move(p1Bowls, p2Bowls, p1Score, leftSteps.head) then
//          player = false
//        else
//          if !move(p2Bowls, p1Bowls, p2Score, leftSteps.head) then
//            player = true
//      leftSteps = leftSteps.tail
//    p1Score.value
}

