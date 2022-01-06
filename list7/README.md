# List 7

## Network Scanner

Tests made with 254 hosts.

`InetAddress.getByName(host).isReachable(timeout)` / system call `ping`

- SequentialNetworkScanner - 259 sec / 139 sec
- ThreadedNetworkScanner - 23 sec / 20 sec
- FutureNetworkScanner - 32 sec / 20 sec

## Disk Encryptor

2 GB of data. Thread pool limited to 4 threads to avoid `java.lang.OutOfMemoryError: Java heap space` errors.

- SequentialDiskEncryptor - 18 sec
- FutureDiskEncryptor - 3 sec
