package com.github.utaal.m68k.ast

import com.github.utaal.m68k.{Size, B, W, L}

sealed trait Operand
sealed trait Const extends Operand
case class Immed(value: Literal) extends Const
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

sealed trait Literal
case class IntLiteral(value: Int) extends Literal
case class CharLiteral(value: Char) extends Literal

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


