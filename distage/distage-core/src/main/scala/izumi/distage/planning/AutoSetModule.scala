package izumi.distage.planning

import izumi.distage.model.definition.BootstrapModuleDef
import izumi.distage.model.reflection.universe.RuntimeDIUniverse.Tag
import izumi.distage.model.planning.PlanningHook

abstract class AutoSetModule extends BootstrapModuleDef {
  def register[T: Tag]: AutoSetModule = {
    many[T]
    many[PlanningHook].add(new AssignableFromAutoSetHook[T, T](identity))
    this
  }

  def register[T: Tag, B: Tag](wrap: T => B): AutoSetModule = {
    many[B]
    many[PlanningHook].add(new AssignableFromAutoSetHook[T, B](wrap))
    this
  }
}

object AutoSetModule {
  def apply(): AutoSetModule = new AutoSetModule {}
}