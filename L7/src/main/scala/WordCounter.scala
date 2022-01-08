import scala.annotation.tailrec
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object WordCounter {

  def countTime[A](task : => A) : Long = {
    val time = System.nanoTime()
    task
    System.nanoTime() - time
  }

  def countWord(lines : List[String], word : String) : Int = {
    @tailrec
    def countWordHelper(lines : List[String], counter : Int) : Int = {
      if lines == Nil then counter
      else countWordHelper(lines.tail, counter + processLine(lines.head.trim().split("\\s++").toList, word))
    }
    countWordHelper(lines, 0)
  }

  def countWordParallel(lines : List[String], word : String) : Int = {
    val linesCounter = for( line <- lines ) yield Future {
      processLine(line.trim().split("\\s++").toList, word)
    }
    linesCounter.map(Await.result(_, Duration.Inf)).sum
  }

  def processLine(words : List[String], word : String) : Int = {
    @tailrec
    def processLineHelper(words : List[String], counter : Int) : Int = {
      if words == Nil then counter
      else {
        if words.head == word then processLineHelper(words.tail, counter + 1)
        else processLineHelper(words.tail, counter)
      }
    }
    processLineHelper(words, 0)
  }

  def processFile(path : String) : Unit = {
    val source = Source.fromFile(path)
    val lines : List[String] = source.getLines().toList
    source.close()
    print("SEQUENTIAL:  ")
    println(countTime(countWord(lines, "this")))
    print("PARALLEL:    ")
    println(countTime(countWordParallel(lines, "this")))
  }


  def main(args: Array[String]) : Unit = {
    println("SAMPLE 1 ---------------")
    println("File is medium size (581 kB)")
    processFile("sample1.txt")
    println("")

    println("SAMPLE 2 ---------------")
    println("File is large (26 MB)")
    processFile("sample2.txt")
    println("")

    println("SAMPLE 3 ---------------")
    println("File is small (1,6 kB)")
    processFile("sample3.txt")
    println("------------------------")
  }

}
