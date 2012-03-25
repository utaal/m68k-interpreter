package com.github.utaal.m68k

import com.github.utaal.m68k.ast._

object bitwise {
  implicit def mask(size: Size) = new {
    private def getMask: Long = size match {
      case B => 0xffL
      case W => 0xffffL
      case L => 0xffffffffL
    }

    def mask(value: Long): Long = value & getMask
    def reverseMask(value: Long): Long = value & (0xffffffffL ^ getMask)
  }
}

import bitwise._

case class RegisterState(val value: Long) {
  require (value >= 0L && value <= 0xffffffffL)
  def get(size: Size): Long = size mask value
  def set(size: Size, newValue: Long) =
    new RegisterState(value - (size mask value) + (size mask newValue))
}

class CPUState {
} 
