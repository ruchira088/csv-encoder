package com.ruchij

object App {
  case class Person(name: Name, age: Int)
  case class Name(firstName: String, lastName: Option[String])

  def main(args: Array[String]): Unit = {}
}
