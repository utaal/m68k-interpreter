package com.github.utaal.m68k

sealed trait Size {
  def bytes: Int
}

object Size {
  sealed trait WL extends Size

  case object B extends Size with WL {
    val bytes = 1
  }
  case object W extends Size with WL {
    val bytes = 2
  }
  case object L extends Size with WL {
    val bytes = 4
  }
}
