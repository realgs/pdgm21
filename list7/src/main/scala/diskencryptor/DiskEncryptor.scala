package diskencryptor

import java.io.File
import java.util.Base64

abstract class DiskEncryptor(val directory: File) {

    private def readBinaryFile(file: File): Array[Byte] = {
        /* Reads the file and returns its contents as an array of bytes. */
        val length = file.length.toInt
        val bytes = new Array[Byte](length)
        val fis = new java.io.FileInputStream(file)
        val bis = new java.io.BufferedInputStream(fis)
        bis.read(bytes, 0, length)
        bis.close()
        bytes
    }

    // Base64 is not encryption
    def encryptFile(file: File): File = {
        /* Encodes file contents with Base64 and writes the result to a new file.
         * Returns the new file.
         */
        println("Encrypting file: " + file.getName)
        // 'encrypt' data with base64
        val data = readBinaryFile(file)
        val encodedBytes = Base64.getEncoder.encode(data)
        // write encoded data to new file
        val encryptedFile = new File(file.getAbsolutePath + ".crypt")
        val fos = new java.io.FileOutputStream(encryptedFile)
        fos.write(encodedBytes)
        fos.close()
        encryptedFile
    }

    def getFiles(dir: File = directory): List[File] = {
        /* Returns a list of all files in the directory tree. Skips directories. */
        if (!dir.exists()) {
            throw new RuntimeException("Directory does not exist: " + dir)
        }
        val files = dir.listFiles.toList
        files.filter(_.isFile) ++ files.withFilter(_.isDirectory).flatMap(getFiles)
    }

    def encrypt(): List[File]
}
