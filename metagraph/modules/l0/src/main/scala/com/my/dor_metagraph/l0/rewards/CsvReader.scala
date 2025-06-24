package com.my.dor_metagraph.l0.rewards
import scala.io.Source
import java.io.InputStream

object CsvReader {

  def readCsvFromResources(fileName: String): List[(String, BigDecimal)] = {
    val stream: InputStream = Option(getClass.getResourceAsStream(s"/$fileName"))
      .getOrElse(throw new RuntimeException(s"Resource file '$fileName' not found"))

    val lines = Source.fromInputStream(stream).getLines().toList

    lines.map { line =>
      line.split(",", 2).toList match {
        case address :: amountStr :: Nil =>
          val amount = BigDecimal(amountStr.trim)
          (address.trim, amount)
        case _ =>
          throw new RuntimeException(s"Invalid line format: $line")
      }
    }
  }
}