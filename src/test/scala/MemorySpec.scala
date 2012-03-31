import com.github.utaal.m68k._
import com.github.utaal.m68k.ast._

import org.specs2._

class MemorySpec extends mutable.Specification {
  "Memory" should {
    "create new immutable instances on set and match" in {
      val mem: Memory = new LinearMemory(1000L)
      val m1 = mem.set(B, 100L, 0xfeL)
      m1.get(B, 100L) should_== 0xfeL
      m1.get(B, 101L) should_== 0x00L
      val m2 = mem.set(L, 100L, 0xdeadbeefL)
      m2.get(L, 100L) should_== 0xdeadbeefL
      m2.get(W, 100L) should_== 0xdeadL
      m2.get(W, 102L) should_== 0xbeefL
      m2.get(B, 101L) should_== 0xadL
      m2.get(B, 105L) should_== 0x00L
    }
  }
}
