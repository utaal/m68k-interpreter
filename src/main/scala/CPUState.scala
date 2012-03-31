package com.github.utaal.m68k

import com.github.utaal.m68k.ast._

object RegisterState {
  def apply(): RegisterState = RegisterState()
}

case class RegisterState(value: Long) {
  require (value >= 0L && value <= 0xffffffffL)
  def getMask(size: Size): Long = size match {
    case B => 0xffL
    case W => 0xffffL
    case L => 0xffffffffL
  }
  def mask(size: Size, value: Long): Long = value & getMask(size)
  def reverseMask(size: Size, value: Long): Long = value & (0xffffffffL ^ getMask(size))

  def get(size: Size): Long = mask(size, value)
  def set(size: Size, newValue: Long) =
    new RegisterState(reverseMask(size, value) | mask(size, newValue))
}

object ProgramCounter {
  def apply(): ProgramCounter = ProgramCounter(0L)
}

case class ProgramCounter(value: Long) {
  require (value >= 0L && value <= 0xffffffL)
}

object StatusRegister {
  def apply(): StatusRegister = StatusRegister(false, false, false, false, false)
}

case class StatusRegister(C: Boolean, V: Boolean, Z: Boolean, N: Boolean, X: Boolean)

object CPUState {
  val DataRegisterNumber = 8
  val AddressRegisterNumber = 8
  val FramePointerRegister = 6
  val StackPointerRegister = 7

  private def makeRegisterArray(num: Int): Vector[RegisterState] = {
    val emptyRegState = RegisterState(0L)
    Vector.fill[RegisterState](num)(emptyRegState)
  }

  def apply(): CPUState = CPUState(
    CPUState.makeRegisterArray(CPUState.DataRegisterNumber),
    CPUState.makeRegisterArray(CPUState.AddressRegisterNumber),
    ProgramCounter(),
    StatusRegister()
  )
}

case class CPUState(
  D: Vector[RegisterState],
  A: Vector[RegisterState],
  PC: ProgramCounter,
  SR: StatusRegister
) {
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

  val FP = A(CPUState.FramePointerRegister)

  def setFP(size: Size, value: Long) =
    setA(CPUState.FramePointerRegister, size, value)

  val SP = A(CPUState.StackPointerRegister)

  def setSP(size: Size, value: Long) =
    setA(CPUState.StackPointerRegister, size, value)

  def setPC(value: Long) =
    this.copy(PC = ProgramCounter(value))

  def setSR(sr: StatusRegister) =
    this.copy(SR = sr)
}

