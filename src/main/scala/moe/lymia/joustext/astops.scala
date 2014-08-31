/*
 * Copyright (C) 2014 Lymia Aluysia <lymiahugs@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is furnished
 * to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

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
    case StaticInstruction(char) =>
      out.append(char)
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
    case Abort(reason) =>
      out.append(",: "+reason+" (.)*"+options.maxCycles+" :,")
    case Comment(text) =>
      out.append(",: "+text+" :,\n")

    case x => throw new ASTException("Tried to generate unknown AST component: "+x.toString())
  }

  // Minimum execution time -- used to figure out when to stop expanding some constructs.
  def minExecTime(ast: Block)(implicit options: GenerationOptions): Int = (ast map {
    case StaticInstruction(_)   => 1
    case Repeat(value, block)   => value * minExecTime(block)
    case While(block)           => 1

    // synthetic instructions
    case Forever(_)             => options.maxCycles
    case IfElse(a, b)           => 1 + math.min(minExecTime(a), minExecTime(b))
    case FromTo(_, f, t, block) => math.max(t.generate - f.generate + 1, 0) * minExecTime(block)
    case Label(_, block)        => minExecTime(block)
    case Break(_)               => 0
    case LetIn(_, block)        => minExecTime(block)
    case Splice(block)          => minExecTime(block)
    case Abort(_)               => options.maxCycles

    case x => throw new ASTException("Tried to find min execution time of unknown AST component: "+x.toString())
  }).sum
}
