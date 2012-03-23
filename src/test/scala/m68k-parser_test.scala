import com.github.utaal.m68k._

import org.specs2._

class M68kParserSpec extends mutable.Specification {
  import ParseResult._

  "The m68k assembly parser" should {
    "correctly parse instructions with immediate values as operand(s)" in {
      "ADD #3"         as   Op("ADD", None, Immed(3) :: Nil)
      "ADD #AA, (A3)"  like { case Op("ADD", None, Label("AA") :: _ :: Nil) => ok }
    }
    "correctly parse instr. with register addressing as operand(s)" in {
      "MOVE (A0), D2"  as   Op("MOVE", None, Indirect(Address(0)) :: Direct(Data(2)) :: Nil)
      "MOVE -(A0), D3" as   Op("MOVE", None, IndirDecr(Address(0)) :: Direct(Data(3)) :: Nil)
    }
  }

  object ParseResult {
    class ParseResult(a: String) {
      def as(b: Op) = M68kParser.parse(a) must beSome.which (_ must_== b)
      def like(b: PartialFunction[Op, org.specs2.matcher.MatchResult[_]]) =
        M68kParser.parse(a) must beSome.like(b)
    }
    implicit def parseResult(s: String): ParseResult = new ParseResult(s)
  }
}

