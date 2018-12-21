package com.github.pshirshov.izumi.distage.planning.extensions

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicReference

import com.github.pshirshov.izumi.distage.model.plan.ExecutableOp.ProxyOp
import com.github.pshirshov.izumi.distage.model.plan.{DodgyPlan, ExecutableOp, OrderedPlan, SemiPlan}
import com.github.pshirshov.izumi.distage.model.planning.{PlanAnalyzer, PlanningObserver}
import com.github.pshirshov.izumi.distage.model.reflection.universe.RuntimeDIUniverse
import com.github.pshirshov.izumi.fundamentals.graphs.dotml.Digraph
import com.github.pshirshov.izumi.fundamentals.platform.language.Quirks._
import distage._

import scala.collection.mutable



class GraphObserver(planAnalyzer: PlanAnalyzer, @Id("gc.roots") roots: Set[DIKey]) extends PlanningObserver {
  private val beforeFinalization = new AtomicReference[SemiPlan](null)

  override def onSuccessfulStep(next: DodgyPlan): Unit = {}

  override def onPhase00PlanCompleted(plan: DodgyPlan): Unit = synchronized {
    beforeFinalization.set(null)
  }

  override def onPhase05PreFinalization(plan: SemiPlan): Unit = synchronized {
    beforeFinalization.set(plan)
  }

  override def onPhase10PostFinalization(plan: SemiPlan): Unit = {}

  override def onPhase20Customization(plan: SemiPlan): Unit = {}

  override def onPhase50PreForwarding(plan: SemiPlan): Unit = {}

  override def onPhase90AfterForwarding(finalPlan: OrderedPlan): Unit = synchronized {
    val dotfile = render(finalPlan)
    val name = s"plan-${System.currentTimeMillis()}.gv"
    val path = Paths.get(s"target", name)
    val last = Paths.get(s"target", "plan-last.gv")

    Files.write(path, dotfile.getBytes(StandardCharsets.UTF_8)).discard()
    Files.deleteIfExists(last).discard()
    Files.createLink(last, path).discard()
  }

  private def render(finalPlan: OrderedPlan): String = {
    val km = new KeyMinimizer(finalPlan.keys)
    val g = new Digraph(graphAttr = mutable.Map("rankdir" -> "LR"))

    val legend = new Digraph("cluster_legend", graphAttr = mutable.Map("label" -> "Legend", "style" -> "dotted", "rankdir" -> "TB"))
    legend.node("normal", "Regular", mutable.Map("style" -> "filled", "shape" -> "box", "fillcolor" -> "darkolivegreen3"))
    legend.node("weak", "Weak", mutable.Map("style" -> "dashed", "shape" -> "box"))
    legend.node("collected", "Removed by GC", mutable.Map("style" -> "filled", "shape" -> "box", "fillcolor" -> "coral1"))
    legend.node("root", "GC Root", mutable.Map("style" -> "filled", "shape" -> "box", "fillcolor" -> "gold", "peripheries" -> "2"))

    Seq("normal", "weak", "root", "collected").sliding(2).foreach {
      p =>
        legend.edge(p.head, p.last, attrs = mutable.Map("style" -> "invis"))
    }

    val main = new Digraph("cluster_main", graphAttr = mutable.Map("label" -> "Context", "shape" -> "box"))
    val collected = new Digraph("cluster_collected", graphAttr = mutable.Map("label" -> "Collected", "style" -> "dotted"))

    val preGcPlan = beforeFinalization.get()
    val preTopology = planAnalyzer.topology(preGcPlan.steps)

    val removedKeys = preTopology.dependencies.graph.keys
    val goodKeys = finalPlan.topology.dependencies.graph.keys

    val missingKeys = removedKeys.toSet.diff(goodKeys.toSet)
    val missingKeysSeq = missingKeys.toSeq

    goodKeys.foreach {
      k =>

        val rootStyle = if (roots.contains(k)) {
          Map("fillcolor" -> "gold", "peripheries" -> "2")
        } else {
          Map("fillcolor" -> "darkolivegreen3")
        }
        val attrs = mutable.Map("style" -> "filled", "shape" -> "box") ++ rootStyle

        val op = finalPlan.toSemi.index(k)
        val name = km.render(k)
        modify(name, attrs, op)
        main.node(name, attrs = attrs)
    }

    finalPlan.topology.dependencies.graph.foreach {
      case (k, deps) =>
        deps.foreach {
          d =>
            main.edge(km.render(k), km.render(d))
        }
    }

    missingKeysSeq.foreach {
      k =>
        val attrs = mutable.Map("style" -> "filled", "shape" -> "box", "fillcolor" -> "coral1")
        val op = preGcPlan.index(k)
        val name = km.render(k)
        modify(name, attrs, op)
        collected.node(name, attrs = attrs)
    }

    preTopology.dependencies.graph.foreach {
      case (k, deps) =>
        deps.foreach {
          d =>
            if ((missingKeys.contains(k) && !missingKeys.contains(d)) || (missingKeys.contains(d) && !missingKeys.contains(k))) {
              collected.edge(km.render(k), km.render(d), attrs = mutable.Map("color" -> "coral1"))
            } else if (missingKeys.contains(d) && missingKeys.contains(k)) {
              collected.edge(km.render(k), km.render(d))
            }
        }
    }

    g.subGraph(main)
    g.subGraph(collected)
    g.subGraph(legend)
    g.source()
  }

  private def modify(name: String, attrs: mutable.Map[String, String], op: ExecutableOp): Unit = {
    val label = op match {
      case op: ExecutableOp.InstantiationOp =>
        op match {
          case ExecutableOp.CreateSet(_, _, _, _) =>
            "newset"
          case op: ExecutableOp.WiringOp =>
            op.wiring match {
              case w: RuntimeDIUniverse.Wiring.UnaryWiring =>
                w match {
                  case _: RuntimeDIUniverse.Wiring.UnaryWiring.ProductWiring =>
                    "make"
                  case RuntimeDIUniverse.Wiring.UnaryWiring.Function(_, _) =>
                    "lambda"
                  case RuntimeDIUniverse.Wiring.UnaryWiring.Instance(_, _) =>
                    "instance"
                  case RuntimeDIUniverse.Wiring.UnaryWiring.Reference(_, _, weak) =>
                    if (weak) {
                      attrs.put("style", "filled,dashed")
                    }
                    "ref"
                }
              case RuntimeDIUniverse.Wiring.FactoryMethod(_, _, _) =>
                "fdef"

              case RuntimeDIUniverse.Wiring.FactoryFunction(_, _, _) =>
                "ffun"
            }
        }
      case ExecutableOp.ImportDependency(_, _, _) =>
        "import"

      case op: ExecutableOp.ProxyOp =>
        op match {
          case ProxyOp.MakeProxy(_, _, _, byNameAllowed) =>
            if (byNameAllowed) {
              "byname"
            } else {
              "proxy"
            }

          case ProxyOp.InitProxy(_, _, _, _) =>
            "init"

        }


    }
    attrs.put("label", s"$name:=$label").discard()
  }


}

