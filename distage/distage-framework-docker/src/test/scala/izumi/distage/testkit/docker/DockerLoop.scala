package izumi.distage.testkit.docker

import com.typesafe.config.ConfigFactory
import distage._
import izumi.distage.config.AppConfigModule
import izumi.distage.docker.UntypedDockerContainer
import izumi.distage.docker.bundled.{KafkaDocker, KafkaDockerModule, ZookeeperDockerModule}
import izumi.distage.docker.healthcheck.ContainerHealthCheck.HealthCheckResult
import izumi.distage.docker.modules.DockerSupportModule
import izumi.distage.model.definition.ModuleDef
import izumi.distage.model.plan.ExecutableOp
import izumi.distage.model.plan.ExecutableOp.MonadicOp.AllocateResource
import izumi.distage.model.plan.ExecutableOp.WiringOp.CallProvider
import izumi.fundamentals.platform.strings.IzString.toRichIterable
import izumi.logstage.api.routing.ConfigurableLogRouter
import izumi.logstage.distage.LogIOModule
import logstage.{IzLogger, Log}
import zio._
import zio.clock._
import zio.duration.Duration._

import scala.collection.mutable

sealed trait ContainerState
object ContainerState {
  final case class Unknown() extends ContainerState
  final case class Running() extends ContainerState
  final case class Failed() extends ContainerState
}
case class ContainerStatus(container: UntypedDockerContainer, state: ContainerState)

class MutableStateRegistry(logger: IzLogger) {
  private val registry = mutable.HashMap.empty[DIKey, ContainerStatus]

  def all(): Seq[(DIKey, ContainerStatus)] = registry.toSeq
  def set(id: DIKey, s: ContainerState): Unit = {
    registry.get(id) match {
      case Some(value) =>
        set(id, value.copy(state = s))

      case None =>
        ???
    }
  }
  def set(id: DIKey, status: ContainerStatus): Unit = {
    val oldStatus = registry.put(id, status)

    val newState = status.state
    val oldState = oldStatus.map(_.state).getOrElse(ContainerState.Unknown())
    if (newState != oldState) {
      logger.info(s"[$id]: State transition: $oldState -> $newState")
    }
  }
}

object DockerLoop {

  def main(args: Array[String]): Unit = {
    val injector = Injector[Task]()
    val router = ConfigurableLogRouter(threshold = Log.Level.Info)
    val logger = IzLogger(router)
    val module = new ModuleDef {
      include(LogIOModule[Task](ConfigurableLogRouter(threshold = Log.Level.Warn), setupStaticLogRouter = false))
      include(DockerSupportModule[Task])
      include(AppConfigModule(ConfigFactory.load()))

      include(ZookeeperDockerModule[Task])
      include(KafkaDockerModule[Task])
    }
    val registry = new MutableStateRegistry(logger)

    val action = for {
      _ <- Task(logger.info(s"Next iteration"))
      _ <- Task(logger.info(s"Registry: ${registry.all().niceList() -> "known containers"}"))
      _ <- sleep(Finite(500))
      plan <- Task {
        injector.plan(PlannerInput(module, Activation.empty, Roots.apply(DIKey[KafkaDocker.Container])))
      }
      index = plan.steps.map(s => (s.target, s)).toMap
      containerSteps = plan.steps.filter(op => isDockerOp(op))
      containerDeps = containerSteps.map {
        op =>
          val effect = asDockerOpUnsafe(index)(op)
          //(op.target, effect.wiring.requiredKeys.map(index.apply).filter(isDockerOp).map(asDockerOpUnsafe(index)).map(_.target))
          (op.target, effect.wiring.requiredKeys.map(index.apply))
      }

      // outer health check, not necessary, just a demo of the "perception" step
      _ <- Task {
        registry.all().foreach {
          case (id, cs) =>
            val result = cs.container.containerConfig.healthCheck.checkUnsafe(logger, cs.container)
            result match {
              case healthcheck: HealthCheckResult.BadHealthcheck =>
                logger.info(s"Healthcheck failed: $id -> $result")
                registry.set(id, ContainerState.Failed())

              case healthcheck: HealthCheckResult.GoodHealthcheck =>
                registry.set(id, ContainerState.Running())
            }
        }
      }
      // actual healthchecks and restoration happen here
      _ <- injector.produce(plan).use {
        loc =>
          containerSteps.foreach {
            step =>
              val newStatus = ContainerStatus(loc.index(step.target).asInstanceOf[UntypedDockerContainer], ContainerState.Running())
              registry.set(step.target, newStatus)
          }
          Task.succeed(())
      }
    } yield {}

    val loop = action.forever

    println(zio.Runtime.default.unsafeRun(loop))
  }

  private def asDockerOpUnsafe(ops: Map[DIKey, ExecutableOp])(op: ExecutableOp): CallProvider = {
    val effectOp = op.asInstanceOf[AllocateResource].effectKey
    val effect = ops(effectOp).asInstanceOf[CallProvider]
    effect.asInstanceOf[CallProvider]
  }

  private def isDockerOp(op: ExecutableOp) = {
    op.target.tpe.<:<(SafeType.get[UntypedDockerContainer])
  }
}
