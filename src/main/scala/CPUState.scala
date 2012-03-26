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

case class RegisterState(value: Long) {
  require (value >= 0L && value <= 0xffffffffL)
  def get(size: Size): Long = size mask value
  def set(size: Size, newValue: Long) =
    new RegisterState((size reverseMask value) | (size mask newValue))
}

case class ProgramCounter(value: Long) {
  require (value >= 0L && value <= 0xffffffL)
}

case class StatusRegister(C: Boolean, V: Boolean, Z: Boolean, N: Boolean, X: Boolean)

object CPUState {
  val DataRegisterNumber = 8
  val AddressRegisterNumber = 8
  val FramePointerRegister = 6
  val StackPointerRegister = 7
}

case class CPUState(
  D: Vector[RegisterState],
  A: Vector[RegisterState],
  PC: ProgramCounter,
  SR: StatusRegister
) {
  val dataRegisters = (0 to 8) map (RegisterState(_))
  
  private def updatedRegVector(vec: Vector[RegisterState], num: Int, size: Size, value: Long) =
    vec.updated(num, vec(num).set(size, value))

  def setD(number: Int, size: Size, value: Long) = {
    require (number >= 0 && number < CPUState.DataRegisterNumber)
    this.copy(D = updatedRegVector(D, number, size, value))
  }

  def setA(number: Int, size: Size, value: Long) = {
    require (number >= 0 && number < CPUState.AddressRegisterNumber)
    this.copy(A = updatedRegVector(A, number, size, value))
  }

  def setFP(size: Size, value: Long) =
    setA(CPUState.FramePointerRegister, size, value)

  def setSP(size: Size, value: Long) =
    setA(CPUState.StackPointerRegister, size, value)
}

