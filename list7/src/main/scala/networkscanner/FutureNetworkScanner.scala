package networkscanner

import networkscanner.NetworkUtils.pingArray
import org.apache.commons.net.util.SubnetUtils

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class FutureNetworkScanner(subnet: SubnetUtils, timeout: Int) extends ConcurrentNetworkScanner {
    override def scan(): List[(String, String)] = {
        /* Split the subnet into n chunks, where n is the number of available processors.
         * Then, for each chunk, create a Future that will scan the chunk and return the results.
         * Finally, return an iterator that will iterate over the results of the futures.
         */
        val hosts = subnet.getInfo.getAllAddresses
        val grouped = hosts.grouped(MAX_THREADS)
        val threads: Future[List[Array[(String, String)]]] = Future.traverse(grouped.toList)(hosts => Future {
            pingArray(hosts, timeout)
        })
        // flatten the list of lists of tuples into a single list of tuples
        Await.result(threads, Duration.Inf).flatten
    }
}
