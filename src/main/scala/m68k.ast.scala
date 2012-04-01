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

sealed trait Literal

object Literal {
  case class Int(value: scala.Int) extends Literal
  case class Char(value: scala.Char) extends Literal
  case class Label(name: String) extends Literal
}

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

sealed trait InstructionAddressing extends Operand

object InstructionAddressing {
  sealed trait Relative extends InstructionAddressing
  object Relative {
    case class Label(name: String) extends Relative
  }
}

sealed trait Op {
  def opcode: String
}

sealed trait SizedOp extends Op {
  def size: Size
}

package DataOps {
  class Binary(
    val opcode: String, val size: Size, val src: DataAddressing, val dest: DataAddressing)
    extends SizedOp {
    override def toString =
      "Binary(%s,%s,%s,%s)" format (opcode, size.toString, src.toString, dest.toString)
    override def equals(other: Any): Boolean = other match {
      case b:Binary => opcode == b.opcode && size == b.size && src == b.src && dest == b.dest
      case _ => false
    }
  }

  object Binary {
    def apply(opcode: String, size: Size, src: DataAddressing, dest: DataAddressing) =
      new Binary(opcode, size, src, dest)
  }

  case class MOVE(
    override val size: Size,
    override val src: DataAddressing,
    override val dest: DataAddressing) extends Binary("MOVE", size, src, dest)

  case class BinaryA(
    override val opcode: String,
    override val size: Size.WL,
    override val src: DataAddressing,
    override val dest: DataAddressing.Direct.Address) extends Binary(opcode, size, src, dest)

  case class BinaryI(
    override val opcode: String,
    override val size: Size,
    override val src: DataAddressing.Immediate,
    override val dest: DataAddressing) extends Binary(opcode, size, src, dest)
}

package ControlOps {
  class Unary(
    val opcode: String, val dest: InstructionAddressing) extends Op

  object Unary {
    def apply(opcode: String, dest: InstructionAddressing) = new Unary(opcode, dest)
  }
}

sealed trait Line

case class Section(name: String, org: Int, lines: List[Line])

sealed trait DirectiveLine extends Line
object DirectiveLine {
  case class Equ(label: String, value: Int) extends DirectiveLine
  case class DC(label: Option[String], size: Size, values: List[Literal]) extends DirectiveLine
  case class DS(label: Option[String], size: Size, count: Int) extends DirectiveLine
  case class End(label: Option[String], entryPoint: String) extends DirectiveLine
}

case class OpLine(label: Option[String], op: Op) extends Line

case class Program(sections: List[Section])

