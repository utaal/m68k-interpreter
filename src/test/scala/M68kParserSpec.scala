import com.github.utaal.m68k._
import com.github.utaal.m68k.ast._

import org.specs2._

class M68kParserSpec extends mutable.Specification {
  implicit def ParsableString(a: String) = new {
    import com.github.utaal.m68k.{M68kParser => pp}
    def as = new {
      def opLine(lbl: Option[String], op: Op) =
        pp.parseAll(pp.opLine, a) must beLike {
          case pp.Success(res, _) => res must_== OpLine(lbl, op)
        }
    }
    /*def like = new {*/
    /*  def opLine(b: PartialFunction[Line, org.specs2.matcher.MatchResult[_]]) =*/
    /*    pp.parseAll(pp.opLine, a) must beLike { case pp.Success(res, _) => res must beLike(b) }*/
    /*}*/
  }

  "The M68kParser.opLine production" should {
    "correctly parse instructions with immediate values as operand" in {
      "ADDI.W #3, D2".as.opLine(None, DataOps.BinaryI(
        "ADDI",
        Size.W,
        DataAddressing.Immediate(Literal.Int(3)),
        DataAddressing.Direct.Data(Register.Data(2))
      ))
    }

    "correctly parse instr. with register addressing as operand(s)" in {
      "MOVE.L (A0), D2".as.opLine(None, DataOps.MOVE(
        Size.L,
        DataAddressing.Indirect(Register.Address(0)),
        DataAddressing.Direct.Data(Register.Data(2))
      ))

      "MOVEA.W -(A0), A3".as.opLine(None, DataOps.BinaryA(
        "MOVEA",
        Size.W,
        DataAddressing.IndirDecr(Register.Address(0)),
        DataAddressing.Direct.Address(Register.Address(3))
      ))

      "SUB.L 2(A3, D2.W), D3".as.opLine(None, DataOps.Binary(
        "SUB",
        Size.L,
        DataAddressing.IdxBaseDispl(2, Register.Address(3), Register.Data(2), Size.W),
        DataAddressing.Direct.Data(Register.Data(3))
      ))
    }
  }

  /*  "correctly parse sized instructions" in {*/
  /*    "ADD.W D3, D4" as OpLine(None, Op(*/
  /*      "ADD", Some(Size.W), Direct(Register.Data(3)) :: Direct(Register.Data(4)) :: Nil*/
  /*    ))*/
  /*  }*/

  /*  "correctly parse instructions with label" in {*/
  /*    "lbl: JNZ #abc" as OpLine(Some("lbl"), Op("JNZ", None, Label("abc") :: Nil))*/
  /*  }*/

  /*  "correctly parse compiler directives" in {*/
  /*    "lbl: EQU 12" as LabeledDirectiveLine("lbl", Equ(12))*/
  /*    "lbl: DS.W 3" as LabeledDirectiveLine("lbl", DS(Size.W, 3))*/
  /*    "ORG 1010" as DirectiveLine(Org(1010))*/
  /*  }*/
  /*}*/
}

