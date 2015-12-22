package scraper.plans.logical

import scala.reflect.runtime.universe.WeakTypeTag
import scala.util.Try

import scraper.Row
import scraper.exceptions.{LogicalPlanUnresolved, TypeCheckException}
import scraper.expressions._
import scraper.expressions.functions._
import scraper.plans.QueryPlan
import scraper.reflection.fieldSpecFor
import scraper.types.{IntegralType, StructType}

trait LogicalPlan extends QueryPlan[LogicalPlan] {
  def resolved: Boolean = (expressions forall (_.resolved)) && childrenResolved

  def childrenResolved: Boolean = children forall (_.resolved)

  def strictlyTypedForm: Try[LogicalPlan] = Try {
    this transformExpressionsDown {
      case e => e.strictlyTypedForm.get
    }
  }

  lazy val strictlyTyped: Boolean = resolved && (strictlyTypedForm.get sameOrEqual this)

  def childrenStrictlyTyped: Boolean = children forall (_.strictlyTyped)

  def sql: String

  def select(projections: Seq[Expression]): LogicalPlan = Project(this, projections map {
    case e: NamedExpression => e
    case e                  => e as e.sql
  })

  def select(first: Expression, rest: Expression*): LogicalPlan = select(first +: rest)

  def filter(condition: Expression): LogicalPlan = Filter(this, condition)

  def limit(n: Expression): LogicalPlan = Limit(this, n)

  def limit(n: Int): LogicalPlan = this limit lit(n)
}

trait InconvertibleToSQL extends LogicalPlan {
  override def sql: String = throw new UnsupportedOperationException(
    s"Instances of ${getClass.getSimpleName} cannot be mapped to SQL"
  )
}

trait UnresolvedLogicalPlan extends LogicalPlan {
  override def output: Seq[Attribute] = throw new LogicalPlanUnresolved(this)

  override def resolved: Boolean = false
}

trait LeafLogicalPlan extends LogicalPlan {
  override def children: Seq[LogicalPlan] = Seq.empty
}

trait UnaryLogicalPlan extends LogicalPlan {
  def child: LogicalPlan

  override def children: Seq[LogicalPlan] = Seq(child)
}

trait BinaryLogicalPlan extends LogicalPlan {
  def left: LogicalPlan

  def right: LogicalPlan

  override def children: Seq[LogicalPlan] = Seq(left, right)
}

case class UnresolvedRelation(name: String) extends LeafLogicalPlan with UnresolvedLogicalPlan {
  override def nodeCaption: String = s"${getClass.getSimpleName} $name"

  override def sql: String = name
}

case object EmptyRelation extends LeafLogicalPlan with InconvertibleToSQL {
  override def output: Seq[Attribute] = Nil
}

case object SingleRowRelation extends LeafLogicalPlan {
  override val output: Seq[Attribute] = Nil

  override def sql: String = ""
}

case class NamedRelation(child: LogicalPlan, tableName: String) extends UnaryLogicalPlan {
  override def output: Seq[Attribute] = child.output

  override def sql: String = s"FROM `$tableName`"
}

case class LocalRelation(data: Iterable[Row], schema: StructType)
  extends LeafLogicalPlan with InconvertibleToSQL {

  override val output: Seq[Attribute] = schema.toAttributes

  override def nodeCaption: String =
    s"${getClass.getSimpleName} ${output map (_.annotatedString) mkString ", "}"
}

object LocalRelation {
  def apply[T <: Product: WeakTypeTag](data: Iterable[T]): LocalRelation = {
    val schema = fieldSpecFor[T].dataType match { case t: StructType => t }
    val rows = data.map { product => Row.fromSeq(product.productIterator.toSeq) }
    LocalRelation(rows, schema)
  }
}

case class Project(child: LogicalPlan, projections: Seq[NamedExpression])
  extends UnaryLogicalPlan {

  assert(projections.nonEmpty, "Project should have at least one expression")

  override def expressions: Seq[Expression] = projections

  override lazy val output: Seq[Attribute] = projections map (_.toAttribute)

  override def nodeCaption: String =
    s"${getClass.getSimpleName} ${projections map (_.annotatedString) mkString ", "}"

  override def sql: String = s"SELECT ${projections map (_.sql) mkString ", "} ${child.sql}"
}

case class Filter(child: LogicalPlan, condition: Expression) extends UnaryLogicalPlan {
  override lazy val output: Seq[Attribute] = child.output

  override def nodeCaption: String = s"${getClass.getSimpleName} ${condition.annotatedString}"

  override def sql: String = s"${child.sql} WHERE ${condition.sql}"
}

case class Limit(child: LogicalPlan, limit: Expression) extends UnaryLogicalPlan {
  override lazy val output: Seq[Attribute] = child.output

  override lazy val strictlyTypedForm: Try[LogicalPlan] = for {
    n <- limit.strictlyTypedForm map {
      case IntegralType(e) if e.foldable            => e
      case IntegralType.Implicitly(e) if e.foldable => e
      case _ =>
        throw new TypeCheckException("Limit must be an integral constant")
    }
  } yield if (n sameOrEqual limit) this else copy(limit = n)

  override def nodeCaption: String = s"${getClass.getSimpleName} ${limit.annotatedString}"

  override def sql: String = s"${child.sql} LIMIT ${limit.sql}"
}

trait JoinType {
  def sql: String
}

case object Inner extends JoinType {
  override def sql: String = "INNER"
}

case object LeftSemi extends JoinType {
  override def sql: String = "LEFT SEMI"
}

case object LeftOuter extends JoinType {
  override def sql: String = "LEFT OUTER"
}

case object RightOuter extends JoinType {
  override def sql: String = "RIGHT OUTER"
}

case object FullOuter extends JoinType {
  override def sql: String = "FULL OUTER"
}

case class Join(
  left: LogicalPlan,
  right: LogicalPlan,
  joinType: JoinType,
  maybeCondition: Option[Expression]
) extends BinaryLogicalPlan {
  override lazy val output: Seq[Attribute] = joinType match {
    case LeftSemi   => left.output
    case Inner      => left.output ++ right.output
    case LeftOuter  => left.output ++ right.output.map(_.?)
    case RightOuter => left.output.map(_.?) ++ right.output
    case FullOuter  => left.output.map(_.?) ++ right.output.map(_.?)
  }

  override def nodeCaption: String = {
    val details = joinType.toString +: maybeCondition.map(_.annotatedString).toSeq mkString ", "
    s"${getClass.getSimpleName} $details"
  }

  override def sql: String = {
    s"${left.sql} ${joinType.sql} JOIN ${right.sql}"
  }
}

case class Subquery(child: LogicalPlan, alias: String) extends UnaryLogicalPlan {
  override lazy val output: Seq[Attribute] = child.output

  override def nodeCaption: String = s"${getClass.getSimpleName} $alias"

  override def sql: String = s"(${child.sql}) AS $alias"
}

case class Aggregate(
  child: LogicalPlan,
  groupingExpressions: Seq[Expression],
  aggregateExpressions: Seq[NamedExpression]
) extends UnaryLogicalPlan {

  override def sql: String = {
    val aggregations = aggregateExpressions map (_.sql) mkString ", "
    val groupings = groupingExpressions map (_.sql) mkString ", "
    s"SELECT $aggregations ${child.sql} GROUP BY $groupings"
  }

  override lazy val output: Seq[Attribute] = aggregateExpressions map (_.toAttribute)
}

case class Sort(child: LogicalPlan, order: Seq[SortOrder]) extends UnaryLogicalPlan {
  override def output: Seq[Attribute] = child.output

  override def sql: String = s"ORDER BY ${order mkString ", "}"
}
