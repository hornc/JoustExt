package moe.lymia.joustext

object astops {
  import ast._, astextension._

  final case class GenerationOptions(maxCycles: Int = 100000)

  // AST Printing
  def generate(ast: Block)(implicit options: GenerationOptions) = {
    val buf = new java.lang.StringBuilder()
    printAst(ast, buf)
    buf.toString
  }
  def printAst(ast: Block, out: Appendable)(implicit options: GenerationOptions): Unit = ast foreach {
    case StaticInstruction(char) => out.append(char)
    case Repeat(value, block) =>
      if(value < 0) throw new ASTException("Negative repeat count!")
      out.append("(")
      printAst(block, out)
      out.append(")*")
      out.append(value.generate.toString)
    case While(block) =>
      out.append("[")
      printAst(block, out)
      out.append("]")

    case _ => throw new ASTException("Tried to generate unknown AST component: "+ast.toString())
  }

  // Minimum execution time -- used to figure out when to stop expanding some constructs.
  def minExecTime(ast: Block)(implicit options: GenerationOptions): Int = (ast map {
    case StaticInstruction(_)   => 1
    case Repeat(value, block)   => value * minExecTime(block)
    case While(block)           => 1

    // synthetic instructions
    case Assign(_, block)       => minExecTime(block)
    case Forever(_)             => options.maxCycles
    case IfElse(a, b)           => 1 + math.min(minExecTime(a), minExecTime(b))
    case If(block)              => minExecTime(block)
    case IfNot(block)           => minExecTime(block)
    case FromTo(_, f, t, block) => math.max(t.generate - f.generate + 1, 0) * minExecTime(block)
    case Label(_, block)        => minExecTime(block)
    case Break(_)               => 0
    case LetIn(_, block)        => minExecTime(block)

    case _ => throw new ASTException("Tried to find min execution time of unknown AST component: "+ast.toString())
  }).sum
}
