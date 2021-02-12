package com.ruchij

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Witness}

trait CsvEncoder[-A] {
  def encode(value: A): Seq[String]

  val fields: Seq[String] = Seq("")
}

object CsvEncoder {
  def apply[A](implicit csvEncoder: CsvEncoder[A]): CsvEncoder[A] = csvEncoder

  implicit class CsvEncoderOps[A](value: A) {
    def toCsv(implicit csvEncoder: CsvEncoder[A]): Seq[(String, String)] =
      csvEncoder.fields.zip(csvEncoder.encode(value))
  }

  implicit def genericCsvEncoder[A, Repr <: HList](
    implicit labelledGeneric: LabelledGeneric.Aux[A, Repr],
    labelledGenericCsvEncoder: CsvEncoder[Repr]
  ): CsvEncoder[A] =
    new CsvEncoder[A] {
      override def encode(value: A): Seq[String] =
        labelledGenericCsvEncoder.encode(labelledGeneric.to(value))

      override val fields: Seq[String] = labelledGenericCsvEncoder.fields
    }

  implicit val hNilCsvEncoder: CsvEncoder[HNil] = new CsvEncoder[HNil] {
    override def encode(value: HNil): Seq[String] = Seq.empty

    override val fields: Seq[String] = Seq.empty
  }

  implicit def hlistCsvEncoder[K <: Symbol, V, Tail <: HList](
    implicit witness: Witness.Aux[K],
    valueCsvEncoder: CsvEncoder[V],
    tailCsvEncoder: CsvEncoder[Tail]
  ): CsvEncoder[FieldType[K, V] :: Tail] =
    new CsvEncoder[FieldType[K, V] :: Tail] {
      override def encode(value: FieldType[K, V] :: Tail): Seq[String] =
        value match {
          case head :: tail => valueCsvEncoder.encode(head) ++ tailCsvEncoder.encode(tail)
        }

      override val fields: Seq[String] =
        valueCsvEncoder.fields.map(
          fieldName => if (fieldName.isEmpty) witness.value.name else s"${witness.value.name}/$fieldName"
        ) ++ tailCsvEncoder.fields
    }

  implicit val stringCsvEncoder: CsvEncoder[String] =
    (value: String) => Seq(value)

  implicit def numericCsvEncoder[A: Numeric]: CsvEncoder[A] = (number: A) => Seq(number.toString)

  implicit val booleanCsvEncoder: CsvEncoder[Boolean] = (value: Boolean) => Seq(value.toString)

  implicit def optionCsvEncoder[A](implicit csvEncoder: CsvEncoder[A]): CsvEncoder[Option[A]] =
    new CsvEncoder[Option[A]] {
      override def encode(option: Option[A]): Seq[String] =
        option.fold(csvEncoder.fields.map(_ => ""))(csvEncoder.encode)

      override val fields: Seq[String] = csvEncoder.fields
    }
}
