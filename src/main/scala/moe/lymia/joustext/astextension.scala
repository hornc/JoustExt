package moe.lymia.joustext

object astextension {
  import ast._

  // Value extensions
  final case class Variable(s: String) extends Value
  final case class Add(a: Value, b: Value) extends Value
  final case class Sub(a: Value, b: Value) extends Value
  final case class Mul(a: Value, b: Value) extends Value
  final case class Div(a: Value, b: Value) extends Value

  // comments, etc
  final case class Abort(reason: String) extends SimpleInstruction

  // synthetic instructions
  final case class SyntheticException(s: String) extends ASTException(s)

  trait SyntheticInstruction extends Instruction
  final case class Forever(block: Block) extends SyntheticInstruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }
  final case class IfElse(ifClause: Block, elseClause: Block) extends SyntheticInstruction {
    def transverse(f: Instruction => Block) =
      copy(ifClause   = ifClause  .transverse(f),
           elseClause = elseClause.transverse(f))
  }
  final case class FromTo(name: String, from: Value, to: Value, block: Block) extends SyntheticInstruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }
  final case class Label(label: String, block: Block) extends SyntheticInstruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }
  final case class Break(label: String) extends SimpleInstruction
  final case class Splice(block: Block) extends SyntheticInstruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }

  // functions
  case class Function(params: Seq[String], body: Block)
  final case class LetIn(definitions: Map[String, Function], block: Block) extends SyntheticInstruction {
    def transverse(f: Instruction => Block) = copy(block = block.transverse(f))
  }
  final case class FunctionInvocation(name: String, params: Seq[Value]) extends SimpleInstruction
}