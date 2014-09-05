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

object astextension {
  import ast._

  // value extensions
  final case class Variable(s: String) extends Value
  final case class Add(a: Value, b: Value) extends Value
  final case class Sub(a: Value, b: Value) extends Value
  final case class Mul(a: Value, b: Value) extends Value
  final case class Div(a: Value, b: Value) extends Value

  // comparisons for compile time if/else
  trait Predicate
  final case class Equals     (a: Value, b: Value) extends Predicate
  final case class LessThan   (a: Value, b: Value) extends Predicate
  final case class GreaterThan(a: Value, b: Value) extends Predicate

  final case class Not (v: Predicate)               extends Predicate
  final case class And (a: Predicate, b: Predicate) extends Predicate
  final case class Or  (a: Predicate, b: Predicate) extends Predicate

  // comments, etc
  final case class Abort(reason : String) extends SimpleInstruction
  final case class Raw  (comment: String) extends SimpleInstruction

  // synthetic instructions
  trait SyntheticInstruction extends Instruction
  final case class Assign(vars: Map[String, Value], block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class IfElse(predicate: Predicate, ifClause: Block, elseClause: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) =
      copy(ifClause   = f(ifClause  ),
           elseClause = f(elseClause))
  }
  final case class FromTo(name: String, from: Value, to: Value, block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class Splice(block: Block) extends SyntheticInstruction with SimpleBlock
  final case class Invert(block: Block) extends SyntheticInstruction with SimpleBlock

  final case class CallCC(name: String, block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }

  // functions
  case class Function(params: Seq[String], body: Block)
  final case class LetIn(definitions: Map[String, Function], block: Block) extends SyntheticInstruction {
    def mapContents(f: Block => Block) = copy(block = f(block))
  }
  final case class FunctionInvocation(name: String, params: Seq[Value]) extends SimpleInstruction
}