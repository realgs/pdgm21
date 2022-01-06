package diskencryptor.tests

import java.io.File

import diskencryptor.SequentialDiskEncryptor


object TestsSequentialDiskEncryptor {
    def main(args: Array[String]): Unit = {
        val testDir = new File("testdata").getAbsoluteFile
        val sequentialDiskEncryptor = new SequentialDiskEncryptor(testDir)

        val startTime = System.currentTimeMillis
        val files: List[File] = sequentialDiskEncryptor.encrypt()

        println(s"Encryption took ${(System.currentTimeMillis - startTime) / 1000} seconds")
        println("\nEncrypted files:")
        for (file <- files) {
            println(file.getAbsolutePath)
        }
    }
}
