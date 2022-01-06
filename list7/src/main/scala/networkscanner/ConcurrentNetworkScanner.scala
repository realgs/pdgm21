package networkscanner

trait ConcurrentNetworkScanner extends NetworkScanner {
    final val MAX_THREADS: Int = Runtime.getRuntime.availableProcessors
}
