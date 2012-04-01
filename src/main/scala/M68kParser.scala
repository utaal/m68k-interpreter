package com.github.utaal.m68k

import com.github.utaal.m68k.ast._
import scala.util.parsing.combinator.RegexParsers

object M68kParser extends RegexParsers {
  override val skipWhitespace = false

  object basic {
    def number = regex("""[0-9]+""".r) ^^ { n => n.toInt }
    def size = "." ~> """[BWL]""".r ^^ { s => s match {
      case "B" => Size.B
      case "W" => Size.W
      case "L" => Size.L
    } }
    def wl = "." ~> """[WL]""".r ^^ { s => s match {
      case "W" => Size.W
      case "L" => Size.L
    } }
  }

  object literal extends Parser[Literal] {
    def int = basic.number ^^ { n => Literal.Int(n) }
    def char = elem('\'') ~> regex("""[A-Za-z]""".r) <~ elem('\'') ^^ {
      c => Literal.Char(c.charAt(0))
    }
    def apply(in: Input): ParseResult[Literal] = (int | char)(in)
  }

  def ws = rep(elem(' '))
  def LABEL = regex("""[a-zA-Z_][0-9a-zA-Z_]+""".r) ^^ { label => label.toLowerCase }
  /*def labelImmediate = "#" ~> LABEL ^^ { case label => Label(label) }*/

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
    def indirIncr = register.address <~ "+" ^^ { s => IndirIncr(s) }
    def indirDecr = "-" ~> register.address ^^ { s => IndirDecr(s) }
    def idxDispl = basic.number ~ register.address ^^ { case displ ~ idx=> IdxDispl(displ, idx) }
    def idxBaseDispl =
      basic.number ~ "(" ~ register.address ~ "," ~ ws ~ register.data ~ basic.wl ~ ")" ^^ {
      case displ ~ _ ~ idx ~ _ ~ _ ~ reg ~ size ~ _ => IdxBaseDispl(displ, idx, reg, size)
    }
    def absolute = basic.number ~ basic.wl ^^ { case addr ~ size => Absolute(addr, size) }
  }

  object binaryDataOp {
    import BinaryDataOp._
    import dataAddressing._

    case class Operands(fst: DataAddressing, snd: DataAddressing)

    def opcode[S <: Size](opcodes: Parser[String], size: Parser[S]) = opcodes ~ size ^^ {
      case opcode ~ size => (opcode, size)
    }

    def nonImmediateData =
      directData | indirect | indirIncr | indirDecr | idxDispl | idxBaseDispl | absolute
    def any =
      immediate | direct | indirect | indirIncr | indirDecr | idxDispl | idxBaseDispl | absolute

    def twoParams[P1, P2](fst: Parser[P1], snd: Parser[P2]) =
      ws ~ fst ~ elem(',') ~ ws ~ snd ^^ { case _ ~ fst ~ _ ~ _ ~ snd => (fst, snd) }

    def move = opcode("MOVE", basic.size) ~ twoParams(any, nonImmediateData) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => MOVE(size, fst, snd)
    }
    def binaryAOpcodes = ("MOVEA" | "ADDA" | "SUBA" | "CMPA")
    def binaryA =
      opcode(binaryAOpcodes, basic.wl) ~ twoParams(any, directAddress) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => BinaryA(opcode, size, fst, snd)
    }
    def binaryAluOpcodes = "ADD" | "SUB" | "AND" | "OR" | "CMP"
    def binaryAluSrcData =
      opcode(binaryAluOpcodes, basic.size) ~ twoParams(directData, nonImmediateData) ^^ {
      case ((opcode, size)) ~ ((fst, snd)) => BinaryDataOp(opcode, size, fst, snd)
    }
  }


  /*def operand =*/
  /*  const | direct | indir | indir_incr | indir_decr | index_displ | index_base_displ | absolute*/
  /*def OPCODE = """[A-Z]+""".r*/
  /*def lineLabel = LABEL <~ ":"*/
  /*def op_1 = OPCODE ~ (SIZE?) ~ ws ~ operand ^^ {*/
  /*  case opcode ~ size ~ _ ~ soperand => Op(opcode, size, List(soperand))*/
  /*}*/
  /*def op_2 = op_1 ~ "," ~ ws ~ operand ^^ {*/
  /*  case Op(opcode, size, operands) ~ _ ~ _ ~ soperand =>*/
  /*    Op(opcode, size, operands :+ soperand)*/
  /*}*/
  /*def opLine = (lineLabel?) ~ ws ~ (op_2 | op_1) ^^ { case label ~ _ ~ op => OpLine(label, op) }*/

  /*def org = "ORG" ~ ws ~ basic.number ^^ { case _ ~ _ ~ value => Org(value) }*/
  /*def end = "END" ~ ws ~ labelImmediate ^^ { case _ ~ _ ~ lbl => End(lbl) }*/

  /*def equ = "EQU" ~ ws ~ basic.number ^^ { case _ ~ _ ~ value => Equ(value) }*/
  /*def ds = "DS" ~ SIZE ~ ws ~ basic.number ^^ { case _ ~ size ~ _ ~ num => DS(size, num) }*/
  /**/
  /*def directiveLine = (org | end) ^^ { d => DirectiveLine(d) }*/
  /*def labeledDirectiveLine = lineLabel ~ ws ~ (equ | ds) ^^ {*/
  /*  case lbl ~ _ ~ dir => LabeledDirectiveLine(lbl, dir)*/
  /*}*/

  /*def line = (opLine | directiveLine | labeledDirectiveLine)*/

  /*def parseLine(in: String) = parseAll(line, in) match {*/
  /*  case Success(res, _) => Some(res)*/
  /*  case f:Failure => error(f.toString()); None*/
  /*}*/
}
