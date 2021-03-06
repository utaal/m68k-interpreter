package com.github.utaal.m68k

import com.github.utaal.m68k.ast._

trait Memory {
  def memSize: Long
  def set(size: Size, addr: Long, value: Long): Memory
  def get(size: Size, addr: Long): Long
  def dump: String
}

object LinearMemory {
  val log = com.github.utaal.log.Logger.for_(this)
}

class LinearMemory private (override val memSize: Long, memory: Vector[Char]) extends Memory {
  import LinearMemory.log._

  require(memSize > 0 && memSize <= Int.MaxValue)

  def this(memSize: Long) =
    this(memSize, Vector.fill(memSize.toInt)(0x00))

  private def getNumChars(addr: Int, num: Int) = {
    val slice = memory.slice(addr, addr + num)
    (slice.head.toLong /: slice.tail)((c, i) => (c << 8) | i)
  }

  def get(size: Size, addr: Long) = {
    require(addr > 0 && addr <= memSize)
    getNumChars(addr.toInt, size.bytes)
  }

  def set(size: Size, addr: Long, value: Long) = {
    require(addr > 0 && addr <= memSize)
    require(value > 0 && value <= 0xffffffffL)
    val len = size.bytes
    val vec = Vector.tabulate[Char](len)(i => ((value >> (8 * (len - 1 - i))) & 0xffL).toChar)
    new LinearMemory(memSize, memory.patch(addr.toInt, vec, size.bytes))
  }

  def dump: String = {
    def hexString(grp: Seq[Char]) = grp.map("%02X" format _.toInt).mkString(" ")
    def printableString(grp: Seq[Char]) =
      grp.map { x => if (x.isLetterOrDigit) x else "?" }.mkString("")
    memory.view.grouped(8).zipWithIndex.filter(_._1.exists(_ != 0)).map {
      case (x, i) => ("%8d" format (i * 8)) + "    " + hexString(x) + "    " + printableString(x)
    } mkString "\n"
  } 
}
