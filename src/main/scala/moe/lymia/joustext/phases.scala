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
  final case class InvokeContinuation(name: String) extends SimpleInstruction
  def evaluateExpressions(i: Block, vars: Map[String, Int], functions: Map[String, Option[Function]])
                         (implicit options: GenerationOptions): Block = i.transverse {
    // function evaluation
    case LetIn(definitions, block) =>
      evaluateExpressions(block, vars, functions ++ definitions.map(x => x.copy(_2 = Some(x._2))))
    case CallCC(name, block) =>
      CallCC(name, evaluateExpressions(block, vars, functions + ((name, None))))

    case FunctionInvocation(name, params) =>
      if(!functions.contains(name)) throw FunctionCallException("No such function: "+name)
      functions.get(name).get match {
        case Some(function) =>
          if(function.params.length != params.length)
            throw FunctionCallException("Called function "+name+" with "+params.length+" parameters. "+
              "("+function.params.length+" expected.)")

          val newValues = function.params.zip(params.map(x => evaluateValue(x, vars))).toMap
          evaluateExpressions(function.body, vars ++ newValues, functions)
        case None =>
          if(params.length != 0)
            throw FunctionCallException("Continuation "+name+" does not take parameters")
          InvokeContinuation(name)
      }

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
    case IfElse(predicate, ifClause, elseClause) =>
      if(evaluatePredicate(predicate, vars)) evaluateExpressions(ifClause, vars, functions)
      else evaluateExpressions(elseClause, vars, functions)

    // fallback case
    case x => x.transverse(x => evaluateExpressions(x, vars, functions))
  }

  // Turn the complex functions into normal BF Joust code!
  // This is basically the core of JoustExt.

  final case class SavedCont(
    block: Block, state: (SavedCont, Map[String, SavedCont])) extends SyntheticInstruction {

    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  val abort = SavedCont(Abort("eof"), (null, Map()))

  // Minimum execution time -- used to figure out when to stop expanding some constructs.
  def minExecTime(ast: Block)(implicit options: GenerationOptions): Int = (ast map {
    case StaticInstruction(_)  => 1
    case Repeat(value, block)  => value * minExecTime(block)
    case While(block)          => 1
    case Abort(_)              => options.maxCycles
    case Raw(_)                => 0
    case Forever(_)            => options.maxCycles

    // synthetic instructions that still exist after splice
    case SavedCont(block, _)   => options.maxCycles
    case InvokeContinuation(_) => options.maxCycles
    case Reset(block)          => minExecTime(block)
    case CallCC(name, block)   => minExecTime(block)

    case x => throw new ASTException("Tried to find min execution time of unknown AST component: "+x.toString)
  }).sum

  // TODO Debug the crap out of.
  def linearize(blk: Block, lastCont: SavedCont = abort, conts: Map[String, SavedCont] = Map(), cycles: Int = 0)
               (implicit options: GenerationOptions): Block = {
    val result = blk.tails.foldLeft((cycles, Seq[Instruction]())) {
      case (t, Seq()) => t
      case ((minCycles, processed), i :: left) =>
        def continuation = SavedCont(left :+ lastCont, (lastCont, conts))
        def buildContinuation(headInst: Block) =
          SavedCont(headInst ++ left :+ lastCont, (lastCont, conts))
        def appendInstruction(newInst: Block) =
          (minCycles + minExecTime(newInst), processed ++ newInst)

        if(minCycles == -1) (minCycles, processed)
        else if(minCycles > options.maxCycles) (-1, processed)
        else i match {
          case `abort` => (-1, processed ++ abort.block)
          case SavedCont(block, (saved, oldConts)) =>
            // TODO I have no idea if this is correct. Figure this out better.
            (-1, processed ++ linearize(block, saved, oldConts, minCycles))
          case Repeat(value, block) =>
            if(value.generate == 0) (minCycles, processed)
            else appendInstruction(
              Repeat(value, linearize(block, buildContinuation(Repeat(value - 1, block)), conts, minCycles)))
          case While(block) =>
            appendInstruction(While(linearize(block, buildContinuation(While(block)), conts, minCycles + 1)))
          case Forever(block) =>
            (-1, processed :+ Forever(linearize(block, buildContinuation(Forever(block)), conts, minCycles)))
          case x: Abort => (-1, x)

          case Reset(block) =>
            appendInstruction(linearize(block, abort, conts, minCycles + 1))
          case CallCC(name, block) =>
            val nextBlock = linearize(block, continuation, conts + ((name, continuation)), minCycles)
            appendInstruction(nextBlock)
          case InvokeContinuation(name) =>
            if(!conts.contains(name)) throw new ASTException("Unknown continuation "+name)
            val SavedCont(block, (lastCont, n_labels)) = conts.get(name).get
            (-1, processed ++ linearize(block, lastCont, n_labels, minCycles))

          case x => (minCycles + minExecTime(x), processed :+ x)
        }
    }
    result._2
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
    PhaseDef("linearize", "Transforms constructs such as if/else into BF Joust code", (b, g) => linearize(b)(g))

    // TODO: Optimization phases that get rid of nonsense like (eof)*10
  )
  def runPhase(p: PhaseDef, b: Block)(implicit options: GenerationOptions) = p.fn(b, options)
}
