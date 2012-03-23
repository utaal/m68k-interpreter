import scala.util.parsing.combinator.RegexParsers

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

object InputParser extends RegexParsers {
  override val skipWhitespace = false
  val WS = rep(" ")
  val immediate = "#" ~ """[0-9]+""".r ^^ { s => val (_ ~ n) = s; Immed(n.toInt) }
  val label = "#" ~ """[A-Z][0-9A-Z]+""".r ^^ { s => val (_ ~ label) = s; Label(label) }
  val const = immediate | label
  val DATA_REG = "D" ~ """[0-9]+""".r ^^ { s => val (_ ~ n) = s; Data(n.toInt) }
  val ADDR_REG = "A" ~ """[0-9]+""".r ^^ { s => val (_ ~ n) = s; Address(n.toInt) }
  val register = (DATA_REG | ADDR_REG) ^^ { s => Direct(s) }
  val indir = "(" ~ register ~ ")" ^^ { s => val (_ ~ Direct(reg) ~ _) = s; Indirect(reg) }
  val indir_incr = indir ~ "+" ^^ { s => val (Indirect(reg) ~ _) = s; IndirIncr(reg) }
  val indir_decr = "-" ~ indir ^^ { s => val (_ ~ Indirect(reg)) = s; IndirDecr(reg) }
  val DISPL = regex("""[0-9]+"""r) ^^ { s => s.toInt }
  val index_displ = DISPL ~ indir ^^ { s => val (displ ~ Indirect(reg)) = s; IdxDispl(displ, reg) }
  val SIZE = "." ~ """[BWL]""".r ^^ { s => val (_ ~ size) = s; size match {
    case "B" => B
    case "W" => W
    case "L" => L
  } }
  var sized_register = register ~ SIZE ^^ { s => val (Direct(reg) ~ size) = s; SizedRegister(reg, size) }
  val index_base_displ = DISPL ~ "(" ~ register ~ "," ~ WS ~ sized_register ~ ")" ^^ { s =>
    val (displ ~ _ ~ Direct(idx) ~ _ ~ _ ~ sizedReg ~ _) = s
    IdxBaseDispl(displ, sizedReg, idx)
  }
  val ADDRESS = regex("""[0-9]+"""r) ^^ { s => s.toInt }
  val absolute = ADDRESS ~ SIZE ^^ { s => val (addr ~ size) = s; Absolute(addr, size) }
  val operand = const | register  | indir | indir_incr | indir_decr | index_displ | index_base_displ | absolute
  val OPCODE = """[A-Z]+""".r
  val op_1 = OPCODE ~ (SIZE?) ~ WS ~ operand ^^ { s =>
    val (opcode ~ size ~ _ ~ soperand) = s
    Op(opcode, size, List(soperand))
  }
  val op_2 = op_1 ~ "," ~ WS ~ operand ^^ { s =>
    val (Op(opcode, size, operands) ~ _ ~ _ ~ soperand) = s
    Op(opcode, size, operands :+ soperand)
  }
  val line = op_2 | op_1

  def parse(in: String) = parseAll(line, in) match {
    case Success(res, _) => res
    case f:Failure => throw new Exception(f.toString)
  }
}

object describe__ {
  def apply[T <: AnyRef](t: T)(implicit m: scala.reflect.Manifest[T]) = println("t was " + t.toString + " of class " + t.getClass.getName() + ", erased from " + m.erasure)
}

/*val lines = io.Source.stdin.getLines*/
val lines = List(
  "ADD #3"
, "ADD.W D3"
, "MUL D3, D2"
, "MUL -(D3)"
, "MUL 2(A3, D2.W), 33.W"
)
for (l <- lines) println(InputParser.parse(l));

def tp(parser: InputParser.Parser[_], s: String) =
  println(InputParser.parseAll(parser, s))
tp(InputParser.operand, "D3")
val operands = List(
  "#1000"
, "#AAA"
)
for (o <- operands) tp(InputParser.operand, o)
//tp(InputParser.line, "MUL D3, D2")

