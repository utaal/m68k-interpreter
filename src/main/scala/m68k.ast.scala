package com.github.utaal.m68k.ast

import com.github.utaal.m68k.Size

sealed trait Operand

sealed trait Register {
  def number: Int
}
object Register {
  case class Data(override val number: Int) extends Register
  case class Address(override val number: Int) extends Register
}

sealed trait InstructionAddressing extends Operand
sealed trait DataAddressing extends Operand

object DataAddressing {
  case class Immediate(value: Literal) extends DataAddressing
  sealed trait Direct extends DataAddressing {
    def register: Register
  }
  object Direct {
    case class Data(override val register: Register.Data) extends Direct
    case class Address(override val register: Register.Address) extends Direct
  }
  case class Indirect(register: Register.Address) extends DataAddressing
  case class IndirIncr(register: Register.Address) extends DataAddressing
  case class IndirDecr(register: Register.Address) extends DataAddressing
  case class IdxDispl(displ: Int, index: Register.Address) extends DataAddressing
  case class IdxBaseDispl(
    displ: Int, index: Register.Address, base: Register.Data, baseSize: Size.WL)
    extends DataAddressing
  case class Absolute(addr: Int, size: Size.WL) extends DataAddressing
}

sealed trait Op {
  def opcode: String
  def size: Size
}

class BinaryDataOp(
  val opcode: String, val size: Size, val src: DataAddressing, val dest: DataAddressing) extends Op 

object BinaryDataOp {
  def apply(opcode: String, size: Size, src: DataAddressing, dest: DataAddressing) =
    new BinaryDataOp(opcode, size, src, dest)

  case class MOVE(
    override val size: Size,
    override val src: DataAddressing,
    override val dest: DataAddressing) extends BinaryDataOp("MOVE", size, src, dest)

  case class BinaryA(
    override val opcode: String,
    override val size: Size.WL,
    override val src: DataAddressing,
    override val dest: DataAddressing.Direct.Address) extends BinaryDataOp(opcode, size, src, dest)

}

sealed trait Literal

object Literal {
  case class Int(value: scala.Int) extends Literal
  case class Char(value: scala.Char) extends Literal
}
/*
case class Op(opcode: String, size: Option[Size], operands: List[Operand])

sealed trait LabeledDirective
sealed trait Directive
case class Equ(value: Int) extends LabeledDirective
case class Org(address: Int) extends Directive
case class DC(size: Size, values: List[Literal]) extends LabeledDirective
case class DS(size: Size, count: Int) extends LabeledDirective
case class End(label: Label) extends Directive

sealed trait Line
case class OpLine(label: Option[String], op: Op) extends Line
case class DirectiveLine(directive: Directive) extends Line
case class LabeledDirectiveLine(label: String, labeledDirective: LabeledDirective) extends Line
*/

