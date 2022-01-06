package networkscanner

import networkscanner.NetworkUtils.{ping, pingArray}
import org.apache.commons.net.util.SubnetUtils

import java.net.InetAddress

class SequentialNetworkScanner(subnet: SubnetUtils, timeout: Int) extends NetworkScanner {
    override def scan(): List[(String, String)] = {
        /* Scan the subnet sequentially */
        val ips = subnet.getInfo.getAllAddresses
        pingArray(ips, timeout).toList
    }
}
