package com.github.utaal.m68k

import com.github.utaal.m68k.ast._
import scala.util.parsing.combinator.RegexParsers

object M68kParser extends RegexParsers {
  override val skipWhitespace = false

  object basic {
    def number = regex("""[+-]?[0-9]+""".r) ^^ { n => n.toInt }
    def size = "." ~> """[BWL]""".r ^^ { s => s match {
      case "B" => Size.B
      case "W" => Size.W
      case "L" => Size.L
    } }
    def wl = "." ~> """[WL]""".r ^^ { s => s match {
      case "W" => Size.W
      case "L" => Size.L
    } }
    def label = regex("""[a-zA-Z_][0-9a-zA-Z_]+""".r)
    def eol = ws ~ rep(elem('\n'))
  }

  object literal extends Parser[Literal] {
    def int = basic.number ^^ { n => Literal.Int(n) }
    def char = elem('\'') ~> regex("""[A-Za-z]""".r) <~ elem('\'') ^^ {
      c => Literal.Char(c.charAt(0))
    }
    def label = basic.label ^^ { lbl => Literal.Label(lbl) }
    def apply(in: Input): ParseResult[Literal] = (int | char)(in)
  }

  def ws = rep(elem(' '))

  object register {
    def data = "D" ~> basic.number ^^ { n => Register.Data(n) }
    def address = "A" ~> basic.number ^^ { n => Register.Address(n) }
  }

  object dataAddressing {
    import DataAddressing._

    def immediate = "#" ~> literal ^^ { case n => Immediate(n) }
    def directData = register.data ^^ { s => Direct.Data(s) }
    def directAddress = register.address ^^ { s => Direct.Address(s) }
    def direct: Parser[Direct] = (directData | directAddress)
    def indirect = "(" ~> register.address <~ ")" ^^ { s => Indirect(s) }
    def indirIncr = "(" ~> register.address <~ ")+" ^^ { s => IndirIncr(s) }
    def indirDecr = "-(" ~> register.address <~ ")" ^^ { s => IndirDecr(s) }
    def idxDispl = basic.number ~ "(" ~ register.address ~ ")" ^^ {
      case displ ~ _ ~ idx ~ _ => IdxDispl(displ, idx)
    }
    def idxBaseDispl =
      basic.number ~ "(" ~ register.address ~ "," ~ ws ~ register.data ~ basic.wl ~ ")" ^^ {
      case displ ~ _ ~ idx ~ _ ~ _ ~ reg ~ size ~ _ => IdxBaseDispl(displ, idx, reg, size)
    }
    def absolute = basic.number ~ basic.wl ^^ { case addr ~ size => Absolute(addr, size) }
  }

  object instructionAddressing {
    import InstructionAddressing._

    def relativeLabel = basic.label ^^ { lbl => Relative.Label(lbl) }
  }

  object dataOp extends Parser[SizedOp] {
    import DataOps._
    import dataAddressing._

    private def opcode[S <: Size](opcodes: Parser[String], size: Parser[S]) = opcodes ~ size ^^ {
      case opcode ~ size => (opcode, size)
    }

    private def nonImmediateData =
      directData | indirect | indirIncr | indirDecr | idxDispl | idxBaseDispl | absolute
    private def any =
      indirDecr | indirIncr | immediate | direct | indirect | idxDispl | idxBaseDispl | absolute

    private def twoParams[P1, P2](fst: Parser[P1], snd: Parser[P2]) =
      ws ~ fst ~ elem(',') ~ ws ~ snd ^^ { case _ ~ fst ~ _ ~ _ ~ snd => (fst, snd) }

    def move = opcode("MOVE", basic.size) ~ twoParams(any, nonImmediateData) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => MOVE(size, fst, snd)
    }

    def binaryAOpcodes = "MOVEA" | "ADDA" | "SUBA" | "CMPA"
    def binaryA =
      opcode(binaryAOpcodes, basic.wl) ~ twoParams(any, directAddress) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => BinaryA(opcode, size, fst, snd)
    }

    def binaryAluOpcodes = "ADD" | "SUB" | "AND" | "OR" | "CMP"
    def binaryAluSrcData =
      opcode(binaryAluOpcodes, basic.size) ~ twoParams(directData, nonImmediateData) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => Binary(opcode, size, fst, snd)
    }
    def binaryAluDestData =
      opcode(binaryAluOpcodes, basic.size) ~ twoParams(nonImmediateData, directData) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => Binary(opcode, size, fst, snd)
    }

    def binaryIOpcodes = "ADDI" | "SUBI" | "CMPI" | "ANDI" | "ORI"
    def binaryI =
      opcode(binaryIOpcodes, basic.size) ~ twoParams(immediate, nonImmediateData) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => BinaryI(opcode, size, fst, snd)
    }

    def apply(in: Input): ParseResult[SizedOp] =
      (move | binaryA | binaryAluSrcData | binaryAluDestData | binaryI)(in)
  }

  object controlOp extends Parser[Op] {
    import ControlOps._
    import instructionAddressing._

    def jumpBraOpcodes = "JMP" | "BRA" | "BEQ" | "BNE" | "BLT" | "BGT" | "BLE" | "BGE"
    def jumpBra = jumpBraOpcodes ~ ws ~ relativeLabel ^^ {
      case opcode ~ _ ~ lbl => Unary(opcode, lbl)
    }

    def apply(in: Input): ParseResult[Op] = (jumpBra)(in)
  }

  def op = dataOp | controlOp

  def lineLabel = basic.label <~ ":"
  def opLine = (lineLabel?) ~ ws ~ op ^^ { case lbl ~ _ ~ op => OpLine(lbl, op) }

  object directiveLine extends Parser[DirectiveLine] {
    import DirectiveLine._

    def end = (lineLabel?) ~ ws ~ "END" ~ ws ~ basic.label ^^ {
      case lineLbl ~ _ ~ _ ~ _ ~ lbl => End(lineLbl, lbl)
    }

    def equ = lineLabel ~ ws ~ "EQU" ~ ws ~ basic.number ^^ {
      case lineLbl ~ _ ~ _ ~ _ ~ num => Equ(lineLbl, num)
    }

    private def dcValues = (literal ~ rep((ws ~ "," ~ ws) ~> literal)) ^^ { case l ~ ll => l :: ll }
    def dc = (lineLabel?) ~ ws ~ "DC" ~ basic.size ~ ws ~ dcValues ^^ {
      case lineLbl ~ _ ~ _ ~ size ~ _ ~ values => DC(lineLbl, size, values)
    }

    def ds = (lineLabel?) ~ ws ~ "DS" ~ basic.size ~ ws ~ basic.number ^^ {
      case lineLbl ~ _ ~ _ ~ size ~ _ ~ num => DS(lineLbl, size, num)
    }

    def apply(in: Input): ParseResult[DirectiveLine] = (end | equ | dc | ds)(in)
  }

  def line = opLine | directiveLine

  def sectionHeader = "SECTION" ~ ws ~ basic.label ~ basic.eol ^^ {
    case _ ~ _ ~ name ~ _ => name
  }
  def orgHeader = "ORG" ~ ws ~ basic.number ~ basic.eol ^^ {
    case _ ~ _ ~ num ~ _ => num
  }

  def section = sectionHeader ~ orgHeader ~ rep(line <~ basic.eol) ^^ {
    case sectionName ~ org ~ lines => Section(sectionName, org, lines)
  }

  def program = rep(section) ^^ { s => Program(s) }

  def parseProgram(in: String): Either[String, Program] = parseAll(program, in + "\n") match {
    case Success(res, _) => Right(res)
    case f:Failure => Left(f.toString())
  }

}
