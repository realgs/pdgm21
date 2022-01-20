import java.io._

class Game {
    val board = Array(4,4,4,4,4,4,0,4,4,4,4,4,4,0)
    var player = false

    def copy(): Game = {
        val game = new Game()
        (0 until 14).foreach(i => {game.board(i) = board(i)})
        game.player = player
        game
    }

    def check_if_legal_move(from: Int): Boolean = {
        if from / 7 != (if player then 1 else 0) then false else
        if from < 0 then false else
        if from == 6 then false else
        if board(from) == 0 then false else
        true
    }

    def move(from: Int): Boolean = {
        if !check_if_legal_move(from) then false
        else
            var index = from
            var balls = board(from)
            board(from) = 0
            while (balls > 0)
                index += 1
                // skip opponent's store
                if index == (if player then 6 else 13) then index += 1
                index = index % 14
                board(index) += 1
                balls -= 1
            // store
            if index != (if player then 13 else 6) then player = !player
            // stealing
            if index >= (if !player then 7 else 0) && index < (if !player then 13 else 6) && board(12 - index) != 0 && board(index) == 1 then
                balls = board(index) + board(12 - index)
                board(index) = 0
                board(12 - index) = 0
                board(if !player then 13 else 6) += balls
            true
    }

    def check_if_finished(): Boolean = {
        val sum1 = board(0) + board(1) + board(2) + board(3) + board(4) + board(5)
        val sum2 = board(7) + board(8) + board(9) + board(10) + board(11) + board(12)
        if sum1 == 0 then true else
        if sum2 == 0 then true else
        false
    }

    def final_points(): (Int, Int) = {
        val sum1 = board(0) + board(1) + board(2) + board(3) + board(4) + board(5) + board(6)
        val sum2 = board(7) + board(8) + board(9) + board(10) + board(11) + board(12) + board(13)
        (sum1, sum2)
    }

    def print_raw(): Unit = {
        var i = 0
        while (i < 14)
            print(board(i))
            print(' ')
            i += 1
        print('\n')
    }

    def print_pretty(perspective: Boolean): Unit = {
        if perspective then
            println(f"╔══╦══╦══╦══╦══╦══╦══╦══╗")
            println(f"║  ║${board(5)}%2d║${board(4)}%2d║${board(3)}%2d║${board(2)}%2d║${board(1)}%2d║${board(0)}%2d║  ║")
            println(f"║${board(6)}%2d╠══╬══╬══╬══╬══╬══╣${board(13)}%2d║")
            println(f"║  ║${board(7)}%2d║${board(8)}%2d║${board(9)}%2d║${board(10)}%2d║${board(11)}%2d║${board(12)}%2d║  ║")
            println(f"╚══╩══╩══╩══╩══╩══╩══╩══╝")
            println(f"    |  |  |  |  |  |")
            println(f"    1  2  3  4  5  6")
            println(f"")
        else
            println(f"╔══╦══╦══╦══╦══╦══╦══╦══╗")
            println(f"║  ║${board(12)}%2d║${board(11)}%2d║${board(10)}%2d║${board(9)}%2d║${board(8)}%2d║${board(7)}%2d║  ║")
            println(f"║${board(13)}%2d╠══╬══╬══╬══╬══╬══╣${board(6)}%2d║")
            println(f"║  ║${board(0)}%2d║${board(1)}%2d║${board(2)}%2d║${board(3)}%2d║${board(4)}%2d║${board(5)}%2d║  ║")
            println(f"╚══╩══╩══╩══╩══╩══╩══╩══╝")
            println(f"    |  |  |  |  |  |")
            println(f"    1  2  3  4  5  6")
            println(f"")
    }

    def print_pretty(): Unit = {
        print_pretty(player)
    }
}

class Player {

    def make_move(state: Game): Int = {
        0
    }

    def translate_move(state: Game, hole: Int): Int = {
        if hole < 1 || hole > 6 then -1 else
        if state.player then hole + 6 else hole - 1
    }
}

class HumanPlayer extends Player {
    
    override def make_move(state: Game): Int = {
        print("\u001b[2J")
        println(s"Player ${if state.player then 2 else 1}")
        state.print_pretty()
        println("\nyour move: ")
        var move = -1
        while (move == -1)
            try
                move = scala.io.StdIn.readInt()
            catch
                case _: Exception => {move = -1}
        translate_move(state, move)
    }

}

class ComputerPlayer extends Player {

    override def make_move(state: Game): Int = {
        var best_move = -1
        var best_value = -73
        (1 until 7).foreach(i => {
            val move = translate_move(state, i)
            val game = state.copy()
            if state.check_if_legal_move(move) then
                game.move(move)
                val eval = if game.player == state.player then static_eval(game) else -static_eval(game)
                if eval > best_value then
                    best_value = eval
                    best_move = move
        })
        best_move
    }

    def static_eval(state: Game): Int = {
        val diff = state.board(6) - state.board(13)
        if state.player then -diff else diff
    }

}

class Server(mode: Int) {
    val player1: Player = if mode % 2 == 0 then new HumanPlayer() else new ComputerPlayer()
    val player2: Player = if mode / 2 == 0 then new HumanPlayer() else new ComputerPlayer()

    def run(): Unit = {
        val game = Game()
        while (!game.check_if_finished())
            if !game.player then
                var hole = player1.make_move(game)
                game.move(hole)
            else
                var hole = player2.make_move(game)
                game.move(hole)
        print("\u001b[2J")
        println("Game Finished !")
        println()
        val (score1, score2) = game.final_points()
        if score1 == score2 then
            println("Draw")
        else if score1 > score2 then
            println(s"Player 1 won $score1 - $score2")
        else
            println(s"Player 2 won $score2 - $score1")
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        println("Choose game mode:")
        println("0 - Human     as player 1  VS Human     as player 2")
        println("1 - Computer  as player 1  VS Human     as player 2")
        println("2 - Human     as player 1  VS Computer  as player 2")
        println("3 - Computer  as player 1  VS Computer  as player 2")
        var mode = -1
        while (mode == -1 || mode > 3 || mode < 0)
            try
                mode = scala.io.StdIn.readInt()
                if mode > 3 || mode < 0 then throw new Exception()
            catch
                case _: Exception => {println("no such mode, choose again:"); mode = -1}
        val server = new Server(mode)
        server.run()
    }
}
