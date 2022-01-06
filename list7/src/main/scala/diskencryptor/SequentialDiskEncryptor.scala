package diskencryptor

import java.io.File

class SequentialDiskEncryptor(directory: File) extends DiskEncryptor(directory) {
    override def encrypt(): List[File] = {
        getFiles().map(encryptFile)
    }
}
