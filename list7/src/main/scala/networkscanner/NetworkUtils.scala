package networkscanner

import java.net.InetAddress


object NetworkUtils {

    def ping(host: String, timeout: Int): Boolean = {
        /*Given a host name and a timeout, this method returns true if the host is reachable, false otherwise.*/
        println("Pinging " + host + "...")
//        InetAddress.getByName(host).isReachable(timeout)
        try {
            var cmd = ""
            if (System.getProperty("os.name").startsWith("Windows")) { // For Windows
                cmd = "ping -w " + timeout + " -n 1 " + host
            }
            else { // For Linux and OSX
                cmd = "ping -c 1 " + host
            }
            val myProcess = Runtime.getRuntime.exec(cmd)
            myProcess.waitFor
            if (myProcess.exitValue == 0) true
            else false
        } catch {
            case _: Exception =>
                false
        }
    }

    def pingArray(hosts: Array[String], timeout: Int): Array[(String, String)] = {
        /*Given an array of ip addresses and a timeout, this method returns an array of tuples containing the
        ip address and the host name of reachable hosts.*/
        // Comparing speed of data structures
        // List + for loop         8057 ms
        // Array + for loop        7995 ms
        // Array map + filter      8078 ms
        // ArrayBuffer + for loop  8006 ms

        //val startTime = System.currentTimeMillis
        var result: Array[(String, String)] = Array()
        for (host <- hosts) {
            if (ping(host, timeout)) {
                val hostname = InetAddress.getByName(host).getCanonicalHostName
                result = result :+ (host -> hostname)
            }
        }
        //println("Time taken: " + (System.currentTimeMillis - startTime) + "ms")
        result
    }
}
