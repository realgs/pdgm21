package networkscanner

import org.apache.commons.net.util.SubnetUtils

trait NetworkScanner {
    def scan(): List[(String, String)]
}
