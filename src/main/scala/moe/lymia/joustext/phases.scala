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

object phases {
  import ast._, astextension._, astops._

  def doInvert(i: Block): Block = i.transverse {
    case IncMem => DecMem
    case DecMem => IncMem
    case x => x.transverse(x => doInvert(x))
  }
  def doSplice(i: Block): Block = i.transverse {
    case Splice(x) => x.transverse(x => doSplice(x))
    case Invert(x) => doInvert(x).transverse(x => doSplice(x))
    case x => x.transverse(x => doSplice(x))
  }

  // evaluate values, and various other compile-time macro stuff.
  final case class FunctionCallException(s: String) extends ASTException(s)
  final case class VariableException(s: String) extends ASTException(s)
  def evaluateValue(v: Value, vars: Map[String, Int]): Int = v match {
    case Constant(x) => x
    case Variable(x) =>
      if(!vars.contains(x)) throw VariableException("No such variable $"+x)
      vars.get(x).get

    case Add(x, y) => evaluateValue(x, vars) + evaluateValue(y, vars)
    case Sub(x, y) => evaluateValue(x, vars) - evaluateValue(y, vars)
    case Mul(x, y) => evaluateValue(x, vars) * evaluateValue(y, vars)
    case Div(x, y) => evaluateValue(x, vars) / evaluateValue(y, vars)
  }
  def evaluatePredicate(p: Predicate, vars: Map[String, Int]): Boolean = p match {
    case Equals     (a, b) => evaluateValue(a, vars) == evaluateValue(b, vars)
    case GreaterThan(a, b) => evaluateValue(a, vars) >  evaluateValue(b, vars)
    case LessThan   (a, b) => evaluateValue(a, vars) <  evaluateValue(b, vars)

    case Not(v)    => !evaluatePredicate(v, vars)
    case Or (a, b) => evaluatePredicate(a, vars) || evaluatePredicate(b, vars)
    case And(a, b) => evaluatePredicate(a, vars) && evaluatePredicate(b, vars)
  }
  def evaluateExpressions(i: Block, vars: Map[String, Int], functions: Map[String, Function])
                         (implicit options: GenerationOptions): Block = i.transverse {
    // function evaluation
    case LetIn(definitions, block) =>
      evaluateExpressions(block, vars, functions ++ definitions)
    case FunctionInvocation(name, params) =>
      if(!functions.contains(name)) throw FunctionCallException("No such function: "+name)
      val function = functions.get(name).get
      if(function.params.length != params.length)
        throw FunctionCallException("Called function "+name+" with "+params.length+" parameters. "+
                                    "("+function.params.length+" expected.)")

      val newValues = function.params.zip(params.map(x => evaluateValue(x, vars))).toMap
      evaluateExpressions(function.body, vars ++ newValues, functions)

    // assign
    case Assign(values, block) =>
      evaluateExpressions(block, vars ++ values.map(x => (x._1, evaluateValue(x._2, vars))), functions)

    // reify stuff that uses values
    case FromTo(name, from, to, block) =>
      (evaluateValue(from, vars) to evaluateValue(to, vars)) flatMap {v =>
        evaluateExpressions(block, vars + ((name, v)), functions)
      }
    case Repeat(times, block) =>
      if(evaluateValue(times, vars) < 0) throw new ASTException("Repeat runs negative times!")
      Repeat(evaluateValue(times, vars), evaluateExpressions(block, vars, functions))
    case MacroIfElse(predicate, ifClause, elseClause) =>
      if(evaluatePredicate(predicate, vars)) evaluateExpressions(ifClause, vars, functions)
      else evaluateExpressions(elseClause, vars, functions)

    // fallback case
    case x => x.transverse(x => evaluateExpressions(x, vars, functions))
  }

  // Turn the complex functions into normal BF Joust code!
  // This is basically the core of JoustExt.

  // Gathers
  final case class RawBlock(block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  def isRaw(i: Block): Boolean = i.forall {
    case StaticInstruction(_) => true
    case Abort(_)             => true
    case Raw(_)               => true

    case Repeat(_, block)     => isRaw(block)
    case While(block)         => isRaw(block)
    case Forever(block)       => isRaw(block)
    case Label(_, block)      => isRaw(block)

    case x => false
  }
  def wrapRaw(i: Block): Block = {
    val results = i.foldLeft((Seq[Instruction](), Seq[Instruction]())) { (last, i) =>
      if(isRaw(i)) last.copy(_1 = last._1 :+ i)
      else if(last._1.isEmpty) last.copy(_2 = last._2 :+ i.mapContents(wrapRaw))
      else last.copy(_1 = Seq[Instruction](), _2 = last._2 :+ RawBlock(last._1) :+ i.mapContents(wrapRaw))
    }
    if(results._1.isEmpty) results._2
    else results._2 :+ RawBlock(results._1)
  }

  def linearize(i: Block)(implicit options: GenerationOptions): Block = i flatMap {
    case x => x //throw new ASTException("Unknown AST component "+x+". Cannot linearize.")
  }
  def unwrapRaw(i: Block): Block = i.transverse {
    case RawBlock(x) => x.transverse(x => unwrapRaw(x))
    case x => x.transverse(x => unwrapRaw(x))
  }

  // phase definitions
  type Phase = (Block, GenerationOptions) => Block
  final case class PhaseDef(shortName: String, description: String, fn: Phase)
  val phases = Seq(
    // Preprocessing phase
    PhaseDef("exprs"    , "Evaluates functions, from-to blocks, and the count for repeat blocks",
             (b, g) => evaluateExpressions(b, Map(), Map())(g)),
    PhaseDef("splice"   , "Processes Splice blocks", (b, g) => doSplice(b)),

    // Core compilation phase
    PhaseDef("wrapRaw"  , "Wraps code that doesn't need to be transformed in linearize into RawBlock objects",
             (b, g) => wrapRaw(b)),
    PhaseDef("linearize", "Transforms constructs such as if/else into BF Joust code", (b, g) => linearize(b)(g)),
    PhaseDef("unwrapRaw", "Unwraps code from RawBlock blocks.", (b, g) => unwrapRaw(b))
  )
  def runPhase(p: PhaseDef, b: Block)(implicit options: GenerationOptions) = p.fn(b, options)
}
