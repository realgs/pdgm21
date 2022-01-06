import scala.io.StdIn._

class KalahaUserInterface:

    val conn: KalahaClientConnection = new KalahaClientConnection

    def printMenu =
        println(" ===== KALAHA MENU ====== ")
        println("1) Connect to server")
        println("2) Join game")
        println("3) Show players")
        println("4) Start game")
        println("5) Make move")
        println("6) Quit")

    def mainLoop =
        var answer = 0
        while answer != 6 do
            printMenu
            try
                answer = readInt()
                answer match
                    case 1 =>
                        try
                            conn.connectToServer
                        catch
                            case e: java.net.ConnectException => println("Something went wrong while trying to connect: " + e.getMessage)
                            case e => println(e.getMessage)
                    case 2 =>
                        println("Enter your name:")
                        val name = readLine()
                        conn.name = name
                        try
                            conn.joinGame
                        catch
                            case e => println(e.getMessage)
                    case 3 =>
                        try
                            conn.getPlayers
                        catch
                            case e: IllegalStateException => println(e.getMessage)
                    case 4 =>
                        try
                            conn.startGame
                        catch
                            case e: IllegalStateException => println(e.getMessage)
                    case 5 =>
                        println("Enter hole number:")
                        val holeNumber = readInt()
                        try
                            conn.makeMove(holeNumber)
                        catch
                            case e => println(e.getMessage)
                    case 6 =>
                        println("Quittnig!")
                        conn.disconnect
                    case _ =>
                        println("Incorrect value, try again!")
            catch
                case e: java.lang.NumberFormatException => println("Enter valid number, please!")
        println("Kalaha done, thank you :), Filip Wisniewski 260406")