package networkscanner

import networkscanner.NetworkUtils.pingArray
import org.apache.commons.net.util.SubnetUtils

class ThreadedNetworkScanner(subnet: SubnetUtils, timeout: Int) extends ConcurrentNetworkScanner {
    override def scan(): List[(String, String)] = {
        /* Split the subnet into n chunks, where n is the number of available processors.
         * Each chunk will be scanned in a separate thread.
         * The result of each thread is added to the result iterator.
         */
        val hosts = subnet.getInfo.getAllAddresses
        val grouped = hosts.grouped(MAX_THREADS)
        var results = List[(String, String)]()

        val threads = grouped.map(hosts => {
            new Thread(() => {
                val result = pingArray(hosts, timeout)
                results = results ++ result
            })
        }).toList

        println("Starting " + threads.length + " threads...")
        threads.foreach(_.start())

        println("Waiting for threads to finish...")
        for (thread <- threads) {
            thread.join()
            println("Thread " + thread.getId + " finished")
        }
        results
    }
}
