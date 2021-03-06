package scraper.expressions

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

import scraper.Row
import scraper.exceptions.TypeMismatchException
import scraper.expressions.Cast.promoteDataType
import scraper.types.{BooleanType, DataType}

trait BinaryLogicalPredicate extends BinaryOperator {
  override def dataType: DataType = BooleanType

  override lazy val strictlyTyped: Try[Expression] = {
    val checkBranch: Expression => Try[Expression] = {
      case BooleanType.Implicitly(e) => Success(promoteDataType(e, BooleanType))
      case e => Failure(
        new TypeMismatchException(e, BooleanType.getClass)
      )
    }

    for {
      lhs <- left.strictlyTyped flatMap checkBranch
      rhs <- right.strictlyTyped flatMap checkBranch
      newChildren = lhs :: rhs :: Nil
    } yield if (sameChildren(newChildren)) this else makeCopy(newChildren)
  }
}

case class And(left: Expression, right: Expression) extends BinaryLogicalPredicate {
  override def nullSafeEvaluate(lhs: Any, rhs: Any): Any = {
    lhs.asInstanceOf[Boolean] && rhs.asInstanceOf[Boolean]
  }

  override def operator: String = "AND"
}

case class Or(left: Expression, right: Expression) extends BinaryLogicalPredicate {
  override def nullSafeEvaluate(lhs: Any, rhs: Any): Any =
    lhs.asInstanceOf[Boolean] || rhs.asInstanceOf[Boolean]

  override def operator: String = "OR"
}

case class Not(child: Expression) extends UnaryOperator {
  override def dataType: DataType = BooleanType

  override lazy val strictlyTyped: Try[Expression] = for {
    e <- child.strictlyTyped map {
      case BooleanType.Implicitly(e) => promoteDataType(e, BooleanType)
      case e                         => throw new TypeMismatchException(e, BooleanType.getClass)
    }
  } yield copy(child = e)

  override def nullSafeEvaluate(value: Any): Any = !value.asInstanceOf[Boolean]

  override def operator: String = "NOT"

  override protected def template(childString: String): String = s"($operator $childString)"
}

case class If(condition: Expression, yes: Expression, no: Expression) extends Expression {
  override protected def strictDataType: DataType = yes.dataType

  override def children: Seq[Expression] = Seq(condition, yes, no)

  override lazy val strictlyTyped: Try[If] = for {
    strictCondition <- condition.strictlyTyped map {
      case BooleanType.Implicitly(e) => promoteDataType(e, BooleanType)
      case e                         => throw new TypeMismatchException(e, BooleanType.getClass)
    }

    strictYes <- yes.strictlyTyped
    strictNo <- no.strictlyTyped
    finalType <- strictYes.dataType widest strictNo.dataType

    promotedYes = promoteDataType(strictYes, finalType)
    promotedNo = promoteDataType(strictNo, finalType)
    newChildren = strictCondition :: promotedYes :: promotedNo :: Nil
  } yield if (sameChildren(newChildren)) this else copy(
    condition = strictCondition, yes = promotedYes, no = promotedNo
  )

  override def evaluate(input: Row): Any = condition.evaluate(input) match {
    case null  => null
    case true  => yes evaluate input
    case false => no evaluate input
  }
}
