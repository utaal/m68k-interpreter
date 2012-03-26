package com.github.utaal.m68k.ast

import com.github.utaal.m68k.{Size, B, W, L}

sealed trait Operand
sealed trait Const extends Operand
case class Immed(value: Int) extends Const
case class Label(name: String) extends Const

sealed trait Register extends Operand {
  val number: Int
}
case class Data(override val number: Int) extends Register
case class Address(override val number: Int) extends Register

case class SizedRegister(reg: Register, size: Size)

sealed trait Addressing extends Operand
case class Direct(register: Register) extends Addressing
case class Indirect(register: Register) extends Addressing
case class IndirIncr(register: Register) extends Addressing
case class IndirDecr(register: Register) extends Addressing
case class IdxDispl(displ: Int, index: Register) extends Addressing
case class IdxBaseDispl(displ: Int, base: SizedRegister, index: Register) extends Addressing
case class Absolute(addr: Int, size: Size) extends Addressing

case class Op(opcode: String, size: Option[Size], operands: List[Operand])
case class Line(label: Option[String], op: Op)
