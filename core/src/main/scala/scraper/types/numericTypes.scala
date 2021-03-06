package scraper.types

import scraper.expressions.Expression

trait NumericType extends PrimitiveType with OrderedType {
  val numeric: Numeric[InternalType]

  def genericNumeric: Numeric[Any] = numeric.asInstanceOf[Numeric[Any]]
}

object NumericType {
  val defaultType = DoubleType

  def unapply(e: Expression): Option[Expression] = e.dataType match {
    case _: NumericType => Some(e)
    case _              => None
  }

  object Implicitly {
    def unapply(e: Expression): Option[Expression] = e.dataType match {
      case t if t narrowerThan defaultType => Some(e)
      case _                               => None
    }

    def unapply(dataType: DataType): Option[DataType] = dataType match {
      case t if t narrowerThan defaultType => Some(t)
      case _                               => None
    }
  }
}

trait IntegralType extends NumericType {
  val integral: Integral[InternalType]

  def genericIntegral: Integral[Any] = integral.asInstanceOf[Integral[Any]]
}

object IntegralType {
  val defaultType = IntType

  def unapply(e: Expression): Option[Expression] = e.dataType match {
    case _: IntegralType => Some(e)
    case _               => None
  }

  object Implicitly {
    def unapply(e: Expression): Option[Expression] = e.dataType match {
      case t if t narrowerThan defaultType => Some(e)
      case _                               => None
    }

    def unapply(dataType: DataType): Option[DataType] = dataType match {
      case t if t narrowerThan defaultType => Some(t)
      case _                               => None
    }
  }
}

case object ByteType extends IntegralType {
  override type InternalType = Byte

  override val integral: Integral[Byte] = implicitly[Integral[Byte]]

  override val numeric: Numeric[Byte] = implicitly[Numeric[Byte]]

  override val ordering: Ordering[Byte] = implicitly[Ordering[Byte]]

  override def sql: String = "TINYINT"
}

case object ShortType extends IntegralType {
  override type InternalType = Short

  override val integral: Integral[Short] = implicitly[Integral[Short]]

  override val numeric: Numeric[Short] = implicitly[Numeric[Short]]

  override val ordering: Ordering[Short] = implicitly[Ordering[Short]]

  override def sql: String = "SMALLINT"
}

case object IntType extends IntegralType {
  override type InternalType = Int

  override val integral: Integral[Int] = implicitly[Integral[Int]]

  override val numeric: Numeric[Int] = implicitly[Numeric[Int]]

  override val ordering: Ordering[Int] = implicitly[Ordering[Int]]

  override def sql: String = "INT"
}

case object LongType extends IntegralType {
  override type InternalType = Long

  override val integral: Integral[Long] = implicitly[Integral[Long]]

  override val numeric: Numeric[Long] = implicitly[Numeric[Long]]

  override val ordering: Ordering[Long] = implicitly[Ordering[Long]]

  override def sql: String = "BIGINT"
}

trait FractionalType extends NumericType {
  val fractional: Fractional[InternalType]

  def genericFractional: Fractional[Any] = fractional.asInstanceOf[Fractional[Any]]
}

object FractionalType {
  val defaultType = DoubleType

  def unapply(e: Expression): Option[Expression] = e.dataType match {
    case _: FractionalType => Some(e)
    case _                 => None
  }

  object Implicitly {
    def unapply(e: Expression): Option[Expression] = e.dataType match {
      case t if t narrowerThan defaultType => Some(e)
      case _                               => None
    }

    def unapply(dataType: DataType): Option[DataType] = dataType match {
      case t if t narrowerThan defaultType => Some(t)
      case _                               => None
    }
  }
}

case object FloatType extends FractionalType {
  override type InternalType = Float

  override val fractional: Fractional[Float] = implicitly[Fractional[Float]]

  override val numeric: Numeric[Float] = implicitly[Numeric[Float]]

  override val ordering: Ordering[Float] = implicitly[Ordering[Float]]

  override def sql: String = "FLOAT"
}

case object DoubleType extends FractionalType {
  override type InternalType = Double

  override val fractional: Fractional[Double] = implicitly[Fractional[Double]]

  override val numeric: Numeric[Double] = implicitly[Numeric[Double]]

  override val ordering: Ordering[Double] = implicitly[Ordering[Double]]

  override def sql: String = "DOUBLE"
}
