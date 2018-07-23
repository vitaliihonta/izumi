package com.github.pshirshov.izumi.distage.planning

import com.github.pshirshov.izumi.distage.model.definition.Binding
import com.github.pshirshov.izumi.distage.model.exceptions.UntranslatablePlanException
import com.github.pshirshov.izumi.distage.model.plan.ExecutableOp._
import com.github.pshirshov.izumi.distage.model.plan._
import com.github.pshirshov.izumi.distage.model.planning.{PlanAnalyzer, PlanMergingPolicy}
import com.github.pshirshov.izumi.fundamentals.collections.Graphs

import scala.collection.mutable

sealed trait ConflictResolution

object ConflictResolution {

  final case class Successful(op: ExecutableOp) extends ConflictResolution

  final case class Failed(ops: Set[InstantiationOp]) extends ConflictResolution

}

class PlanMergingPolicyDefaultImpl(analyzer: PlanAnalyzer) extends PlanMergingPolicy {

  override def extendPlan(currentPlan: DodgyPlan, binding: Binding, currentOp: NextOps): DodgyPlan = {
    (currentOp.provisions ++ currentOp.sets.values).foreach {
      op =>
        val target = op.target
        currentPlan.operations.addBinding(target, op)
    }

    currentPlan
  }

  override def finalizePlan(completedPlan: DodgyPlan): SemiPlan = {
    val resolved = completedPlan.operations.mapValues(resolve).toMap
    val allOperations = resolved.values.collect({ case ConflictResolution.Successful(op) => op }).toSeq
    val issues = resolved.collect({ case (k, ConflictResolution.Failed(ops)) => (k, ops) }).toMap

    if (issues.nonEmpty) {
      // TODO: issues == slots, we may apply slot logic here
      throw new UntranslatablePlanException(s"Unresolved operation conflicts:\n${issues.mkString("\n")}", issues)
    }

    // it's not neccessary to sort the plan at this stage, it's gonna happen after GC
    val index = allOperations.map(op => op.target -> op).toMap
    val topology = analyzer.topology(allOperations)

    val imports = topology
      .dependees
      .graph
      .filterKeys(k => !index.contains(k))
      .map {
        case (missing, refs) =>
          missing -> ImportDependency(missing, refs.toSet, None)
      }
      .toMap
    SemiPlan(completedPlan.definition, (imports.values ++ allOperations).toVector)
  }

  override def reorderOperations(completedPlan: SemiPlan): OrderedPlan = {
    val index = completedPlan.index
    val topology = analyzer.topology(completedPlan.steps)
    val sortedKeys = Graphs.toposort.cycleBreaking(
      topology.dependencies.graph
      , Seq.empty
    )

    val sortedOps = sortedKeys.flatMap(k => index.get(k).toSeq)
    OrderedPlan(completedPlan.definition, sortedOps.toVector, topology)
  }

  protected def resolve(operations: mutable.Set[InstantiationOp]): ConflictResolution = {
    operations match {
      case s if s.nonEmpty && s.forall(_.isInstanceOf[CreateSet]) =>
        val ops = s.collect({ case c: CreateSet => c })
        ConflictResolution.Successful(ops.tail.foldLeft(ops.head) {
          case (acc, op) =>
            acc.copy(members = acc.members ++ op.members)

        })
      case s if s.size == 1 =>
        ConflictResolution.Successful(s.head)
      case other =>
        ConflictResolution.Failed(other.toSet)
    }
  }
}



