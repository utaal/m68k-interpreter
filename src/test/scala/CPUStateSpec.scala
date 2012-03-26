import com.github.utaal.m68k._
import com.github.utaal.m68k.ast._

import org.specs2._

class CPUStateSpec extends mutable.Specification {

  "The immutable Register state class" should {
    "correctly store values which fit in the emulated register" in {
      val r0 = RegisterState(0xffffffffL)
      r0.value must be_>(0L)
      RegisterState(0x100000000L) must throwAn[IllegalArgumentException]
      RegisterState(-1L) must throwAn[IllegalArgumentException]
    }

    "produce a new RegisterState when a value is set" in {
      val r0 = RegisterState(0xdeadbeefL)
      r0.set(L, 0xfefefefeL).value must_== 0xfefefefeL
      r0.set(W, 0xfefeL).value must_== 0xdeadfefeL
      r0.set(B, 0xfeL).value must_== 0xdeadbefeL
      r0.set(B, 0xfefeL).value must_== 0xdeadbefeL
    }
  }

  "The CPUState immutable class" should {
    "correctly construct" in {
      val st = CPUState()
      val emptyStatusRegister = StatusRegister(false, false, false, false, false)
      st must beLike {
        case CPUState(dvec, avec, ProgramCounter(0L), emptyStatusRegister) => {
          forall(dvec ++ avec) (_ must beLike { case RegisterState(0L) => ok })
          dvec.length must_== CPUState.DataRegisterNumber
          avec.length must_== CPUState.AddressRegisterNumber
        }
      }
    }
    "correctly produce new instances of itself on register change" in {
      val st = CPUState()
      val st1 = st.setD(3, L, 0xdeadbeefL)
      st1.D(3).value must_== 0xdeadbeefL
      st1.setD(4, L, 0xfefefefeL).D(3).value must_== 0xdeadbeefL
      st.setA(CPUState.AddressRegisterNumber + 1, L, 0) must throwAn[IllegalArgumentException]
      st.setA(4, W, 0xfefefefeL).A(4).value must_== 0xfefeL
      st.setFP(L, 0xafafafafL).FP.value must_== 0xafafafafL
      st.setSP(B, 0xaffa).SP.value must_== 0xfaL
    }
  }

}
