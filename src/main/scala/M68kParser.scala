package com.github.utaal.m68k

import com.github.utaal.m68k.ast._
import scala.util.parsing.combinator.RegexParsers

object M68kParser extends RegexParsers {
  override val skipWhitespace = false
  val NUMBER = regex("""[0-9]+""".r) ^^ { n => n.toInt }
  val intLiteral = NUMBER ^^ { n => IntLiteral(n) }
  val charLiteral = rep("'") ~> regex("""[A-Za-z]""".r) <~ rep("'") ^^ {
    c => CharLiteral(c.charAt(0))
  }
  val literal = intLiteral | charLiteral
  val WS = rep(" ")
  val immediate = "#" ~> literal ^^ { case n => Immed(n) }
  val LABEL = regex("""[a-zA-Z_][0-9a-zA-Z_]+""".r) ^^ { label => label.toLowerCase }
  val labelImmediate = "#" ~> LABEL ^^ { case label => Label(label) }
  val const = labelImmediate | immediate
  val DATA_REG = "D" ~> NUMBER ^^ { n => Data(n) }
  val ADDR_REG = "A" ~> NUMBER ^^ { n => Address(n) }
  val register = (DATA_REG | ADDR_REG) ^^ { s => Direct(s) }
  val indir = "(" ~> register <~ ")" ^^ { case Direct(reg) => Indirect(reg) }
  val indir_incr = indir <~ "+" ^^ { case Indirect(reg) => IndirIncr(reg) }
  val indir_decr = "-" ~> indir ^^ { case Indirect(reg) => IndirDecr(reg) }
  val DISPL = NUMBER ^^ { s => s }
  val index_displ = DISPL ~ indir ^^ { case displ ~ Indirect(reg) => IdxDispl(displ, reg) }
  val SIZE = "." ~ """[BWL]""".r ^^ { s => val (_ ~ size) = s; size match {
    case "B" => B
    case "W" => W
    case "L" => L
  } }
  var sized_register = register ~ SIZE ^^ { case Direct(reg) ~ size => SizedRegister(reg, size) }
  val index_base_displ = DISPL ~ "(" ~ register ~ "," ~ WS ~ sized_register ~ ")" ^^ {
    case displ ~ _ ~ Direct(idx) ~ _ ~ _ ~ sizedReg ~ _ => IdxBaseDispl(displ, sizedReg, idx)
  }
  val ADDRESS = NUMBER
  val absolute = ADDRESS ~ SIZE ^^ { case addr ~ size => Absolute(addr, size) }
  val operand =
    const | register | indir | indir_incr | indir_decr | index_displ | index_base_displ | absolute
  val OPCODE = """[A-Z]+""".r
  val lineLabel = LABEL <~ ":"
  val op_1 = OPCODE ~ (SIZE?) ~ WS ~ operand ^^ {
    case opcode ~ size ~ _ ~ soperand => Op(opcode, size, List(soperand))
  }
  val op_2 = op_1 ~ "," ~ WS ~ operand ^^ {
    case Op(opcode, size, operands) ~ _ ~ _ ~ soperand =>
      Op(opcode, size, operands :+ soperand)
  }
  val opLine = (lineLabel?) ~ WS ~ (op_2 | op_1) ^^ { case label ~ _ ~ op => OpLine(label, op) }

  val org = "ORG" ~ WS ~ NUMBER ^^ { case _ ~ _ ~ value => Org(value) }
  val end = "END" ~ WS ~ labelImmediate ^^ { case _ ~ _ ~ lbl => End(lbl) }

  val equ = "EQU" ~ WS ~ NUMBER ^^ { case _ ~ _ ~ value => Equ(value) }
  val ds = "DS" ~ SIZE ~ WS ~ NUMBER ^^ { case _ ~ size ~ _ ~ num => DS(size, num) }
  
  val directiveLine = (org | end) ^^ { d => DirectiveLine(d) }
  val labeledDirectiveLine = lineLabel ~ WS ~ (equ | ds) ^^ {
    case lbl ~ _ ~ dir => LabeledDirectiveLine(lbl, dir)
  }

  val line = (opLine | directiveLine | labeledDirectiveLine)

  def parseLine(in: String) = parseAll(line, in) match {
    case Success(res, _) => Some(res)
    case f:Failure => error(f.toString()); None
  }
}
