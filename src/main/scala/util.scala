package com.github.utaal.m68k

object util {
  def describe[T <: AnyRef](t: T)(implicit m: scala.reflect.Manifest[T]) =
    println("t was " + t.toString + " of class " + t.getClass.getName() + ", erased from " + m.erasure)
}
