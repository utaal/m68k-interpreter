import com.github.utaal.m68k._
import com.github.utaal.m68k.ast._

import org.specs2._

class M68kParserSpec extends mutable.Specification {
  implicit def ParsableString(a: String) = new {
    def as(b: Line) = M68kParser.parseLine(a) must beSome.which (_ must_== b)
    def like(b: PartialFunction[Line, org.specs2.matcher.MatchResult[_]]) =
      M68kParser.parseLine(a) must beSome.like(b)
  }

  "The m68k assembly parser" should {
    "correctly parse instructions with immediate values as operand(s)" in {
      "ADD #3" as Line(None, Op("ADD", None, Immed(3) :: Nil))
      "ADD #AA, (A3)" like { case Line(None, Op("ADD", None, Label("aa") :: _ :: Nil)) => ok }
    }

    "correctly parse instr. with register addressing as operand(s)" in {
      "MOVE (A0), D2" as Line(None, Op(
        "MOVE", None, Indirect(Address(0)) :: Direct(Data(2)) :: Nil))
      "MOVE -(A0), D3" as Line(None, Op(
        "MOVE", None, IndirDecr(Address(0)) :: Direct(Data(3)) :: Nil))
      "MUL 2(A3, D2.W), D3" as Line(None, Op(
          "MUL",
          None,
          IdxBaseDispl(2, SizedRegister(Data(2), W), Address(3)) :: Direct(Data(3)) :: Nil
        ))
    }

    "correctly parse sized instructions" in {
      "ADD.W D3, D4" as Line(None, Op("ADD", Some(W), Direct(Data(3)) :: Direct(Data(4)) :: Nil))
    }

    "correctly parse instructions with label" in {
      "lbl: JNZ #abc" as Line(Some("lbl"), Op("JNZ", None, Label("abc") :: Nil))
    }
  }
}

