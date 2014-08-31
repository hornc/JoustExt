package moe.lymia.joustext

import language.implicitConversions

object ast {
  class ASTException(s: String) extends RuntimeException(s)

  trait Value {
    def generate: Int = throw new ASTException("Unresolved value: "+this.toString)
  }
  final case class Constant(i: Int) extends Value {
    override def generate = i
  }
  implicit def int2Val(i: Int) = Constant(i)
  implicit def val2Int(v: Value) = v.generate

  trait Instruction {
    def transverse(f: Instruction => Block): Instruction
  }
  trait SimpleInstruction extends Instruction {
    def transverse(f: Instruction => Block) = this
  }
  type Block = Seq[Instruction]
  implicit final class BlockExt(block: Block) {
    def transverse(f: Instruction => Block) = block.flatMap(f)
  }
  implicit def autoWrapBlock(i: Instruction): Block = Seq(i)

  case class StaticInstruction(s: String) extends SimpleInstruction
  val IncPtr = StaticInstruction(">")
  val DecPtr = StaticInstruction("<")
  val IncMem = StaticInstruction("+")
  val DecMem = StaticInstruction("-")

  final case class Repeat(count: Value, block: Block) extends Instruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }
  final case class While(block: Block) extends Instruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }
}