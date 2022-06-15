package com.dynamicp.scala.collection

class ScalaCollectionExamples {
  def main(args: Array[String]): Unit = {
    val country: Set[String] = Set("INDIA", "USA", "UK")
    val cities: Set[List[String]] = Set("INDIA=MUMBAI", "INDIA=PUNE", "INDIA=NAGPUR", "BAN=DHAKA").map(z=>z.split("=").toList)

  }

}
