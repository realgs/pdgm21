import java.util.concurrent.atomic.AtomicInteger
import castor.Context.Simple.global

import cask.Logger.Console.globalLogger
import cask._

object Main:
    
    val ui = new KalahaUserInterface()
    
    def main(args: Array[String]) =
        ui.mainLoop
        
