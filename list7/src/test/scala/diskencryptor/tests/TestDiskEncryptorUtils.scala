package diskencryptor.tests

import diskencryptor.FutureDiskEncryptor

import java.io.File

object TestDiskEncryptorUtils {
    def main(args: Array[String]): Unit = {
        val encryptor = new FutureDiskEncryptor(new File("/tmp/test"))
        encryptor.getFiles(new File(".")).foreach(println)
    }
}
