package com.github.utaal.m68k

import com.github.utaal.m68k.ast._

case class Initialization(
  val symbols: Map[String, Long],
  val memAlloc: Map[Long, (Size, Long)]) {

  def addSymbol(label: String, value: Long) = this.copy(symbols=symbols + (label -> value))

  def addMemAlloc(addr: Long, size: Size, value: Long) =
    this.copy(memAlloc=memAlloc + (addr -> (size, value)))
}

object Initialization {
  def apply(): Initialization = Initialization(Map[String, Long](), Map[Long, (Size, Long)]())

  private def withSymbol(init: Initialization, lbl: Option[String], pos: Long) =
    (lbl map (l => init.addSymbol(l, pos))) getOrElse init

  private def byteValue(literal: Literal, init: Initialization) = literal match {
    case Literal.Int(litValue) => litValue.toLong
    case Literal.Char(litChar) => litChar.toLong
    case Literal.Label(lbl) => init.symbols get lbl getOrElse error("missing symbol")
  }

  private def lineFold(a: (Long, Initialization), line: Line) = {
    val (pos, init) = a
    import DirectiveLine._
    line match {
      case Equ(lbl, value) => (pos, init.addSymbol(lbl, value))
      case DC(lbl, size, values) =>
        ((pos, withSymbol(init, lbl, pos)) /: values) { (b, value) =>
          val (pos, init) = b
          (pos + size.bytes, init.addMemAlloc(pos, size, byteValue(value, init)))
        }
      case DS(lbl, size, count) =>
        (pos + (size.bytes * count), withSymbol(init, lbl, pos))
      case End(_, _) => (pos, init)
      case OpLine(_, _) => (pos, init)
    }
  }

  def from(program: Program): Initialization =
    (Initialization() /: program.sections) { case (init, Section(_, org, lines)) =>
      ((org.toLong, init) /: lines)(lineFold)._2
    }


}
