package diskencryptor.tests

import java.io.File

import diskencryptor.FutureDiskEncryptor


object TestFutureDiskEncryptor {
    def main(args: Array[String]): Unit = {
        val testDir = new File("testdata").getAbsoluteFile
        val futureDiskEncryptor = new FutureDiskEncryptor(testDir)

        val startTime = System.currentTimeMillis
        val files: List[File] = futureDiskEncryptor.encrypt()

        println(s"Encryption took ${(System.currentTimeMillis - startTime) / 1000} seconds")
        println("\nEncrypted files:")
        for (file <- files) {
            println(file.getAbsolutePath)
        }
    }
}
