package scraper.plans.logical

import scala.collection.JavaConverters._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Checkers

import scraper.{InMemoryCatalog, LoggingFunSuite, Row, TestUtils}
import scraper.exceptions.TypeCheckException
import scraper.expressions.{Attribute, Expression}
import scraper.expressions.dsl._
import scraper.expressions.functions._
import scraper.generators.genRandomPartitions
import scraper.plans.logical.LogicalPlanSuite.{ExprNode, PlanNode}
import scraper.plans.logical.dsl._
import scraper.types.{DataType, IntType}

class LogicalPlanSuite extends LoggingFunSuite with TestUtils with Checkers {
  def genExprNode: Gen[ExprNode] = Gen.sized {
    case size if size < 2 => ExprNode(1, Nil)
    case size =>
      for {
        width <- Gen choose (1, size - 1)
        childrenSizes <- genRandomPartitions(size - 1, width)
        children <- Gen sequence (childrenSizes map (Gen.resize(_, genExprNode)))
      } yield ExprNode(1, children.asScala)
  }

  implicit private val arbExprNode = Arbitrary(genExprNode)

  private val resolve = new Analyzer(new InMemoryCatalog)

  test("transformExpressionDown") {
    check {
      PlanNode(_: ExprNode).transformExpressionsDown {
        case e @ ExprNode(_, children) =>
          e.copy(value = children.map(_.value).sum)
      } match {
        case plan: PlanNode =>
          plan.expression.forall {
            case ExprNode(value, Nil)      => value == 0
            case ExprNode(value, children) => value == children.size
          }
      }
    }
  }

  test("transformExpressionUp") {
    check {
      PlanNode(_: ExprNode).transformExpressionsUp {
        case e @ ExprNode(_, children) =>
          e.copy(value = children.map(_.value).sum)
      } match {
        case plan: PlanNode =>
          plan.expression.forall {
            case ExprNode(value, _) => value == 0
          }
      }
    }
  }

  test("limit - type check") {
    def buildLimit(e: Expression): Limit = SingleRowRelation limit e

    checkStrictlyTyped(buildLimit(1))
    checkStrictlyTyped(buildLimit(lit(1L)))

    checkWellTyped(buildLimit(lit(1) + 1))
    checkWellTyped(buildLimit(lit(1) + 1L))
    checkWellTyped(buildLimit("1"))

    assert(!buildLimit("hello").isWellTyped)
    assert(!buildLimit('a).isWellTyped)
  }

  test("set operator - type check") {
    val r1 = LocalRelation.empty('a.int.!)
    val r2 = LocalRelation.empty('a.int.!)
    val r3 = LocalRelation.empty('a.long.!)
    val r4 = LocalRelation.empty('a.long.!, 'b.string.!)

    checkStrictlyTyped(resolve(r1 union r2))
    checkStrictlyTyped(resolve(r1 union (r4 select ('a cast IntType as 'a))))
    checkStrictlyTyped(resolve(r1 union SingleRowRelation.select(1 as 'a)))

    checkWellTyped(resolve(r1 union r3))
    checkWellTyped(resolve(r1 union (r4 select 'a)))

    intercept[TypeCheckException](resolve(r1 union r4))
  }
}

object LogicalPlanSuite {
  case class ExprNode(value: Int, children: Seq[ExprNode]) extends Expression {
    override def dataType: DataType = IntType

    override def evaluate(input: Row): Any = value
  }

  case class PlanNode(expression: Expression) extends LeafLogicalPlan {
    override def output: Seq[Attribute] = Nil

    override def nodeCaption: String = s"${getClass.getSimpleName} ${expression.nodeCaption}"
  }
}
