package networkscanner.tests

import networkscanner.SequentialNetworkScanner
import org.apache.commons.net.util.SubnetUtils

object TestSequentialNetworkScanner {
    def main(args: Array[String]): Unit = {
        println("Testing SequentialNetworkScanner...")
        val scanner = new SequentialNetworkScanner(new SubnetUtils(args(0)), args(1).toInt)

        val startTime = System.currentTimeMillis
        val hosts = scanner.scan()
        val endTime = System.currentTimeMillis

        println("\nReachable hosts:")
        for (host <- hosts) {
            println(host._1 + " (" + host._2 + ")")
        }
        println("\nScanning took " + (endTime - startTime) / 1000 + " seconds")
    }
}
