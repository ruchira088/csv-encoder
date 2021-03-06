package com.ruchij

import com.ruchij.CsvEncoderTest.{NestedCaseClass, SimpleClass}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CsvEncoderTest extends AnyFlatSpec with Matchers {

  val SimpleOne: SimpleClass = SimpleClass("John", Some(22), isAdult = true)
  val SimpleTwo: SimpleClass = SimpleClass("Smith", None, isAdult = false)

  "Encoding values" should "encode simple case class" in {
    val csvEncoder = CsvEncoder[SimpleClass]

    csvEncoder.fields mustBe Seq("name", "age", "isAdult")

    csvEncoder.encode(SimpleOne) mustBe Seq("John", "22", "true")
    csvEncoder.encode(SimpleTwo) mustBe Seq("Smith", "", "false")
  }

  it should "encode a nested case class" in {
    val csvEncoder = CsvEncoder[NestedCaseClass]

    csvEncoder.fields mustBe Seq(
      "left/name",
      "left/age",
      "left/isAdult",
      "right/name",
      "right/age",
      "right/isAdult",
      "value"
    )

    val nestedOne = NestedCaseClass(SimpleOne, Some(SimpleTwo), Some("1,2,3"))
    val nestedTwo = NestedCaseClass(SimpleOne, None, None)

    csvEncoder.encode(nestedOne) mustBe Seq("John", "22", "true", "Smith", "", "false", "1,2,3")
    csvEncoder.encode(nestedTwo) mustBe Seq("John", "22", "true", "", "", "", "")
  }
}

object CsvEncoderTest {
  case class SimpleClass(name: String, age: Option[Int], isAdult: Boolean)
  case class NestedCaseClass(left: SimpleClass, right: Option[SimpleClass], value: Option[String])
}
