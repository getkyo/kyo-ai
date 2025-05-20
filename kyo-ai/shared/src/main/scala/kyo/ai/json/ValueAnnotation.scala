package kyo.ai.json

import scala.annotation.StaticAnnotation

trait ValueAnnotation[T]:
    def value: T

// String validations
case class minLength(value: Int)  extends StaticAnnotation with ValueAnnotation[Int]
case class maxLength(value: Int)  extends StaticAnnotation with ValueAnnotation[Int]
case class pattern(value: String) extends StaticAnnotation with ValueAnnotation[String]

// Number validations
case class minimum(value: BigDecimal)          extends StaticAnnotation with ValueAnnotation[BigDecimal]
case class maximum(value: BigDecimal)          extends StaticAnnotation with ValueAnnotation[BigDecimal]
case class exclusiveMinimum(value: BigDecimal) extends StaticAnnotation with ValueAnnotation[BigDecimal]
case class exclusiveMaximum(value: BigDecimal) extends StaticAnnotation with ValueAnnotation[BigDecimal]
case class multipleOf(value: BigDecimal)       extends StaticAnnotation with ValueAnnotation[BigDecimal]

// Array validations
case class minItems(value: Int)        extends StaticAnnotation with ValueAnnotation[Int]
case class maxItems(value: Int)        extends StaticAnnotation with ValueAnnotation[Int]
case class uniqueItems(value: Boolean) extends StaticAnnotation with ValueAnnotation[Boolean]

// Object validations
case class minProperties(value: Int) extends StaticAnnotation with ValueAnnotation[Int]
case class maxProperties(value: Int) extends StaticAnnotation with ValueAnnotation[Int]

// Title is actually part of the JSON Schema spec
case class title(value: String)       extends StaticAnnotation with ValueAnnotation[String]
case class description(value: String) extends StaticAnnotation with ValueAnnotation[String]
