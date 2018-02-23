package com.github.pshirshov.izumi.idealingua

import com.github.pshirshov.izumi.idealingua.il.{ILParser, ILRenderer}
import fastparse.core.Parsed
import org.scalatest.WordSpec


class ILParTest extends WordSpec {
  "IL parser" should {
    "parse domain definition" in {
      println(new ILParser().identifier.parse("x.y.z"))
      println(new ILParser().domainId.parse("domain x.y.z"))
      println(new ILParser().field.parse("a: map"))
      println(new ILParser().field.parse("a: map[str, str]"))
      println(new ILParser().field.parse("a: map[str, set[x.Y]]"))

      println(new ILParser().aliasBlock.parse("alias x = y"))
      println(new ILParser().enumBlock.parse("enum MyEnum {X Y Zz}"))

      println(new ILParser().mixinBlock.parse("mixin Mixin {}"))
      println(new ILParser().dtoBlock.parse("data Data {}"))
      println(new ILParser().idBlock.parse("id Id {}"))
      println(new ILParser().serviceBlock.parse("service Service {}"))


      val domaindef =
        """domain x.y.z
          |
          |alias x = y
          |
          |enum MyEnum {X Y Zz}
          |
          |mixin Mixin {
          | + Mixin
          | a: B
          | c: x.Y
          |}
          |
          |data Data {
          |+ Mixin
          |+Mixin
          |}
          |
          |id Id {
          |  a: B
          |  b: map[str, set[x.Y]]
          |}
          |service Service {
          | def testMethod(Mixin1, c.d.Mixin2, x.y.Mixin3): (Mixin1, a.b.Mixin3)
          |}
          |""".stripMargin

      new ILParser().fullDomainDef.parse(domaindef) match {
        case Parsed.Success(v, i) =>
          //println(v)
          println(new ILRenderer(v).render())
        case Parsed.Failure(lp, idx, e) =>
          println(lp, idx, e, e.traced)
          println(e.traced.trace)
      }
    }
  }
}
