package com.github.utaal.m68k

sealed trait Size {
  def bytes: Int
}
case object B extends Size {
  val bytes = 1
}
case object W extends Size {
  val bytes = 2
}
case object L extends Size {
  val bytes = 4
}
