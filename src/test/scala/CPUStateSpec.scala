import com.github.utaal.m68k._
import com.github.utaal.m68k.ast._

import org.specs2._

class CPUStateSpec extends mutable.Specification {

  "The immutable Register state class" should {
    "correctly store values which fit in the emulated register" in {
      val r0 = RegisterState(0xffffffffL)
      r0.value must be_>(0L)
      RegisterState(0x100000000L) must throwA[IllegalArgumentException]
      RegisterState(-1L) must throwA[IllegalArgumentException]
    }

    "produce a new RegisterState when a value is set" in {
      val r0 = RegisterState(0xdeadbeefL)
      r0.set(L, 0xfefefefeL).value must_== 0xfefefefeL
      r0.set(W, 0xfefeL).value must_== 0xdeadfefeL
      r0.set(B, 0xfeL).value must_== 0xdeadbefeL
      r0.set(B, 0xfefeL).value must_== 0xdeadbefeL
    }
  }

}
