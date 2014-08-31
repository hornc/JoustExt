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

import ast._, astextension._
import language.postfixOps

object Parser extends scala.util.parsing.combinator.RegexParsers {
  def identifier = "[a-zA-Z]+".r

  // Value
  object valueParsers {
    // Adapted from http://stackoverflow.com/a/11533809/1733590
    def variable   = "$" ~> identifier ^^ Variable
    def constant   = "[0-9]+".r ^^ (_.toInt) ^^ Constant

    def defer(f: (Value, Value) => Value) = (y: Value) => (x: Value) => f(x, y)
    def plus   = "+" ~> term   ^^ defer(Add)
    def minus  = "-" ~> term   ^^ defer(Sub)
    def times  = "*" ~> factor ^^ defer(Mul)
    def divide = "/" ~> factor ^^ defer(Div)

    def join(parse: Value ~ Seq[Value=>Value]) = parse._2.foldLeft(parse._1)((a, f) => f(a))
    def expr  : Parser[Value] = term ~ (plus | minus).*     ^^ join
    def term  : Parser[Value] = factor ~ (times | divide).* ^^ join
    def factor: Parser[Value] = constant | variable | ("(" ~> expr <~ ")")
  }
  def value = valueParsers.constant | valueParsers.variable | ("(" ~> valueParsers.expr <~ ")")
  def expr  = valueParsers.expr

  // Predicate
  object predicateParsers {
    def comparison = ((expr <~ "==") ~ expr ^^ {case x~y => Equals(x, y)}) |
                     ((expr <~ "!=") ~ expr ^^ {case x~y => Not(Equals(x, y))}) |
                     ((expr <~ "<" ) ~ expr ^^ {case x~y => LessThan(x, y)}) |
                     ((expr <~ ">" ) ~ expr ^^ {case x~y => GreaterThan(x, y)}) |
                     ((expr <~ "<=") ~ expr ^^ {case x~y => Not(GreaterThan(x, y))}) |
                     ((expr <~ ">=") ~ expr ^^ {case x~y => Not(LessThan(x, y))})
    def combination = ("!" ~> pred ^^ Not) |
                      ((pred <~ ("or" |"|"|"||")) ~ pred ^^ {case x~y => Or (x, y)}) |
                      ((pred <~ ("and"|"&"|"&&")) ~ pred ^^ {case x~y => And(x, y)})
    def pred: Parser[Predicate] = comparison | combination | ("(" ~> pred <~ ")")
  }
  def pred = predicateParsers.pred

  // Basic instructions
  def basicInstruction = ("." ^^^ Noop  ) |
                         ("+" ^^^ IncMem) |
                         ("-" ^^^ DecMem) |
                         (">" ^^^ IncPtr) |
                         ("<" ^^^ DecPtr)
  def basicBlock       = (("[" ~> block <~ "]")           |
                          ("while" ~> "{" ~> block <~ "}")) ^^ While
  def repeatBlock      =
    (("(" ~> block <~ ")" <~ "*") ~ value ^^ {case x~y => Repeat(y, x)}) |
    (("repeat" ~> "(" ~> valueParsers.expr <~ ")" <~ "{") ~ block <~ "}" ^^ {case x~y => Repeat(x, y)})

  // AST extensions
  def foreverBlock     =
    (("(" ~> block <~ ")" <~ "*" <~ "-1") |
     ("forever" ~> "{" ~> block <~ "}")   ) ^^ Forever
  // TODO Find a way to parse this without massive backtracking
  def ifNotBlock       = "if" ~> "not" ~> "{" ~> block <~ "}" ^^ {x => IfElse(Seq(), x)}
  def ifBlock          = "if" ~> "{" ~> block <~ "}" ^^ {x => IfElse(x, Seq())}
  def ifElseBlock      =
    (("if" ~> "{" ~> block <~ "}" <~ "else" <~ "{") ~ block <~ "}" ^^ {case x~y => IfElse(x, y)}) |
    (("if" ~> "not" ~> "{" ~> block <~ "}" <~ "else" <~ "{") ~ block <~ "}" ^^ {case x~y => IfElse(y, x)})
  def macroIfElse      =
    (("macro" ~> "if" ~> "(" ~> pred <~ ")" <~ "{") ~ block <~ "}" <~ "else" <~ "{") ~ block <~ "}" ^^ {
      case pred~a~b => MacroIfElse(pred, a, b)
    }
  def macroIf          =
    ("macro" ~> "if" ~> "(" ~> pred <~ ")" <~ "{") ~ block <~ "}" ^^ {case x~y => MacroIfElse(x,y,Seq())}
  def ifLikeBlock      = macroIfElse | macroIf | ifElseBlock | ifNotBlock | ifBlock

  def fromToBlock      =
    ((("for" ~> "(" ~> "$" ~> identifier <~ "in") ~ expr <~ "to") ~ expr <~ ")" <~ "{") ~ block <~ "}" ^^ {
      case id~from~to~block => FromTo(id, from, to, block)
    }
  def label            = (identifier <~ ":") ~ ("{" ~> block <~ "}" |
                                                instruction ^^ (x => Seq(x))) ^^ {case x~y => Label(x, y)}
  def break            = "break" ~> identifier ^^ Break

  def functionCall     =
    ("@" ~> identifier <~ "(") ~ repsep(expr, ",") <~ ")" ^^ {case x~y => FunctionInvocation(x, y)}
  def functionDef      =
    (("@" ~> identifier <~ "(") ~ repsep("$" ~> identifier, ",") <~ ")" <~ "{") ~ block <~ "}" ^^ {
      case id~param~block => (id, Function(param, block))
    }
  def letInBlock       = ("let" ~> functionDef.* <~ "in" <~ "{") ~ block <~ "}" ^^ {
    case fns~block => LetIn(fns.toMap, block)
  }
  def inlineFnDef      = "let" ~> functionDef ~ block ^^ {case x~y => LetIn(Seq(x).toMap, y)}

  def splice           = "local" ~> "{" ~> block <~ "}" ^^ Splice
  def abort            = ("abort" ~> "\"[^\"]*\"".r ^^ (x => Abort(x.substring(1, x.length - 1)))) |
                         ("abort" ^^^ Abort("abort instruction encountered"))

  def invertBlock      = "invert" ~> "{" ~> block <~ "}" ^^ Invert

  def comment          =
    (("comment"|"raw") ~> "\"[^\"]*\"".r ^^ (x => Raw(x.substring(1, x.length - 1)))) |
    (("comment"|"raw") ~> "+margins" ~> "\"[^\"]*\"".r ^^ (x => Raw(x.substring(1, x.length - 1).stripMargin)))

  def setCommand       = ("$" ~> identifier <~ "=") ~ expr ^^ {case x~y => (x, y)}
  def setInBlock       = ("set" ~> setCommand.* <~ "in" <~ "{") ~ block <~ "}" ^^ {case x~y => Assign(x.toMap, y)}

  def extInstruction: Parser[Instruction] = foreverBlock | ifLikeBlock | fromToBlock | break | letInBlock |
                                            inlineFnDef | functionCall | splice | abort | comment | setInBlock |
                                            invertBlock | label
  def instruction   : Parser[Instruction] = basicInstruction | basicBlock | repeatBlock | extInstruction
  def block         : Parser[Block]       = instruction*

  def apply(s:String) = parseAll(block, s.replaceAll("//.*", "")) match {
    case Success(nodes, _)   => Left(nodes)
    case NoSuccess(err,next) => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}