import com.github.utaal.m68k._
import com.github.utaal.m68k.ast.Program

import org.specs2._

class InitializationSpec extends mutable.Specification {

  "Initialization class" should {
    "be constructed from a valid parsed Program" in {
      val program = M68kParser.parseProgram(
        """|SECTION DATA
           |ORG 100
           |a: DS.W 10
           |b: DS.L 1
           |c: DC.B 'a'
           |d: DC.L a""".stripMargin).right.get
      val init = Initialization.from(program)
      init.symbols must havePairs("a" -> 100L, "b" -> 120L, "c" -> 124L, "d" -> 125L)
      init.memAlloc must havePairs(124L -> (Size.B, 'a'.toLong), 125L -> (Size.L, 100L))
    }

    "fill memory" in {
      val init = Initialization()
        .addMemAlloc(100, Size.B, 0xcaL)
        .addMemAlloc(101, Size.B, 0xfeL)
        .addMemAlloc(102, Size.W, 0xbabeL)
      val mem = init applyTo (new LinearMemory(200L))
      mem.get(Size.L, 100) should_== 0xcafebabeL
      mem.get(Size.B, 99) should_== 0x00L
      mem.get(Size.B, 104) should_== 0x00L
    }
  }
}
