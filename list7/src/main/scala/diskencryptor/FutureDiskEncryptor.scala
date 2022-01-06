package diskencryptor

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}


class FutureDiskEncryptor(directory: File) extends DiskEncryptor(directory) {
    override def encrypt(): List[File] = {
        // limit to max 4 futures at a time
        val pool = Executors.newFixedThreadPool(4)
        implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(pool)

        // apply enryptFile method to all files in parallel
        val futures: Future[List[File]] = Future.traverse(getFiles())(file => Future {
            encryptFile(file)
        })
        val result = Await.result(futures, Duration.Inf)
        pool.shutdown()
        result
    }
}
