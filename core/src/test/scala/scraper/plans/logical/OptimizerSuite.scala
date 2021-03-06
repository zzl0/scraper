package scraper.plans.logical

import scala.language.implicitConversions

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.util.Pretty
import org.scalatest.prop.Checkers

import scraper.{InMemoryCatalog, LoggingFunSuite, TestUtils}
import scraper.Test.defaultSettings
import scraper.expressions._
import scraper.expressions.Predicate.splitConjunction
import scraper.expressions.dsl._
import scraper.generators.expressions._
import scraper.plans.logical.Optimizer.{CNFConversion, MergeFilters}
import scraper.plans.logical.dsl._
import scraper.trees.{Rule, RulesExecutor}
import scraper.trees.RulesExecutor.{EndCondition, FixedPoint}

class OptimizerSuite extends LoggingFunSuite with Checkers with TestUtils {
  private implicit def prettyExpression(expression: Expression): Pretty = Pretty {
    _ => "\n" + expression.prettyTree
  }

  private def testRule(
    rule: Rule[LogicalPlan], endCondition: EndCondition
  )(f: (LogicalPlan => LogicalPlan) => Unit): Unit = {
    test(rule.getClass.getSimpleName stripSuffix "$") {
      val analyzer = new Analyzer(new InMemoryCatalog)
      val optimizer = new RulesExecutor[LogicalPlan] {
        override def batches: Seq[RuleBatch] = Seq(
          RuleBatch("TestBatch", endCondition, rule :: Nil)
        )
      }

      f(analyzer andThen optimizer)
    }
  }

  testRule(CNFConversion, FixedPoint.Unlimited) { optimizer =>
    implicit val arbPredicate = Arbitrary(genLogicalPredicate(Nil))

    check(
      forAll { predicate: Expression =>
        val optimizedPlan = optimizer(SingleRowRelation filter predicate)
        val conditions = optimizedPlan.collect {
          case _ Filter condition => splitConjunction(condition)
        }.flatten

        conditions.forall {
          _.collect { case _ && _ => () }.isEmpty
        }
      },

      // CNF conversion may potentially expand the predicate significantly and slows down the test
      // quite a bit.  Here we restrict the max size of the expression tree to avoid slow test runs.
      MaxSize(20)
    )
  }

  testRule(MergeFilters, FixedPoint.Unlimited) { optimizer =>
    implicit val arbPredicate = Arbitrary(genPredicate(Nil))

    check { (p1: Expression, p2: Expression) =>
      val optimized = optimizer(SingleRowRelation filter p1 filter p2)
      val conditions = optimized.collect {
        case f: Filter => f.condition
      }

      conditions == Seq(p1 && p2)
    }
  }
}
