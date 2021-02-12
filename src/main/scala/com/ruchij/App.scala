package com.ruchij

import CsvEncoder.CsvEncoderOps

object App
{
  case class Person(name: Name, age: Int)
  case class Name(firstName: String, lastName: Option[String])

  def main(args: Array[String]): Unit = {
    println {
      Person(Name("John", Some("Smith")), 10).toCsv
    }
  }
}
