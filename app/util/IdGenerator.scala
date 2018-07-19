package util

import java.security.MessageDigest

object IdGenerator {
  def getRandomId(option: String = ""): String = {
    val message = option + System.currentTimeMillis()
    MessageDigest.getInstance("MD5").digest(message.getBytes).map("%02x".format(_)).mkString
  }
}
