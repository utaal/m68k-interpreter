package com.github.utaal.m68k.ast

sealed trait Size
case object B extends Size
case object W extends Size
case object L extends Size

sealed trait Operand
sealed trait Const extends Operand
case class Immed(value: Int) extends Const
case class Label(name: String) extends Const

sealed trait Register extends Operand
case class Data(number: Int) extends Register
case class Address(number: Int) extends Register

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
