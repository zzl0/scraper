package scraper

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Checkers

import scraper.Test.defaultSettings
import scraper.generators.plans.logical._
import scraper.generators.types._
import scraper.parser.Parser
import scraper.plans.logical.{LocalRelation, LogicalPlan}

class SQLGenerationSuite extends LoggingFunSuite with Checkers {
  private val context = new LocalContext

  test("foo") {
    val genPlan = for {
      relationNum <- Gen choose (1, 4)
      genRelation = genStructType map (LocalRelation(Seq.empty, _))
      relations <- Gen listOfN (relationNum, genRelation)
      tables = {
        val tableNames = relations.indices map ("table" + _)
        val dataFrames = relations map (new DataFrame(_, context))
        (dataFrames, tableNames).zipped foreach (_.registerAsTable(_))
        tableNames map (context.table(_).queryExecution.analyzedPlan)
      }
      plan <- genLogicalPlan(tables)
    } yield plan

    implicit val arbPlan = Arbitrary(genPlan)

    check { plan: LogicalPlan =>
      println(new Parser().parse(plan.sql).prettyTree)
      true
    }
  }
}
