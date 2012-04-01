import com.github.utaal.m68k._
import com.github.utaal.m68k.ast._

import org.specs2._

class M68kParserSpec extends mutable.Specification {
  implicit def ParsableString(a: String) = new {
    import com.github.utaal.m68k.{M68kParser => pp}
    private def parseMustEqual[T](parser: pp.Parser[T])(exp: T) =
      pp.parseAll(parser, a) must beLike {
        case pp.Success(res, _) => res must_== exp
      }

    def parsesAs(opLine: OpLine) = parseMustEqual(pp.opLine)(opLine)
    def parsesAs(dirLine: DirectiveLine) = parseMustEqual(pp.directiveLine)(dirLine)
    def parsesAs(section: Section) = parseMustEqual(pp.section)(section)
  }

  "The M68kParser.opLine production" should {
    "accept instructions with immediate values as operand" in {
      "ADDI.W #3, D2" parsesAs OpLine(None, DataOps.BinaryI(
        "ADDI",
        Size.W,
        DataAddressing.Immediate(Literal.Int(3)),
        DataAddressing.Direct.Data(Register.Data(2))
      ))
    }

    "accept instr. with register addressing as operand(s)" in {
      "MOVE.L (A0), D2" parsesAs OpLine(None, DataOps.MOVE(
        Size.L,
        DataAddressing.Indirect(Register.Address(0)),
        DataAddressing.Direct.Data(Register.Data(2))
      ))

      "MOVEA.W -(A0), A3" parsesAs OpLine(None, DataOps.BinaryA(
        "MOVEA",
        Size.W,
        DataAddressing.IndirDecr(Register.Address(0)),
        DataAddressing.Direct.Address(Register.Address(3))
      ))

      "SUB.L 2(A3, D2.W), D3" parsesAs OpLine(None, DataOps.Binary(
        "SUB",
        Size.L,
        DataAddressing.IdxBaseDispl(2, Register.Address(3), Register.Data(2), Size.W),
        DataAddressing.Direct.Data(Register.Data(3))
      ))
    }

    "accept unary control ops" in {
      "JMP label" parsesAs OpLine(None, ControlOps.Unary(
        "JMP",
        InstructionAddressing.Relative.Label("label")
      ))
    }

    "accept labeled lines" in {
      "lbl: BGE abc" parsesAs OpLine(Some("lbl"), ControlOps.Unary(
        "BGE",
        InstructionAddressing.Relative.Label("abc")
      ))
    }
  }

  "The M68kParser.directiveLine production" should {
    "accept valid directives" in {
      "end_while: END begin" parsesAs DirectiveLine.End(Some("end_while"), "begin")
      "aa: EQU 4" parsesAs DirectiveLine.Equ("aa", 4)
      "vec: DS.W 3" parsesAs DirectiveLine.DS(Some("vec"), Size.W, 3)
      "hw: DC.B 'h', 'w'" parsesAs DirectiveLine.DC(
        Some("hw"), Size.B, Literal.Char('h') :: Literal.Char('w') :: Nil
      )
    }
  }

  "The M68kParser.section production" should {
    "accept valid program sections" in {
      """|SECTION CODE
         |ORG 100
         |begin: END begin
         |""".stripMargin parsesAs(
           Section("CODE", 100, DirectiveLine.End(Some("begin"), "begin") :: Nil)
         )
    }
  }
}

