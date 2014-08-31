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

  def basicInstruction = ("+" ^^^ IncMem) |
                         ("-" ^^^ DecMem) |
                         (">" ^^^ IncPtr) |
                         ("<" ^^^ DecPtr)
  def basicBlock       = (("[" ~> block <~ "]")           |
                          ("while" ~> "{" ~> block <~ "}")) ^^ While
  def repeatBlock      =
    (("(" ~> block <~ ")" <~ "*") ~ valueParsers.expr ^^ {case x~y => Repeat(y, x)}) |
    (("repeat" ~> "(" ~> valueParsers.expr <~ ")" <~ "{") ~ block <~ "}" ^^ {case x~y => Repeat(x, y)})

  // AST extensions
  def foreverBlock     =
    (("(" ~> block <~ ")" <~ "*" <~ "-1") |
     ("forever" ~> "{" ~> block <~ "}")   ) ^^ Forever
  // TODO Find a way to parse this without massive backtracking
  def ifNotBlock       = "if" ~> "not" ~> "{" ~> block <~ "}" ^^ IfNot
  def ifBlock          = "if" ~> "{" ~> block <~ "}" ^^ If
  def ifElseBlock      = ("if" ~> "{" ~> block <~ "}" ~> "else" ~> "{") ~ block <~ "}" ^^ {case x~y => IfElse(x, y)}
  def ifLikeBlock      = ifNotBlock | ifElseBlock | ifBlock

  def fromToBlock      =
    ((("for" ~> "$" ~> identifier <~ "in") ~ valueParsers.expr <~ "to") ~ valueParsers.expr <~ "{") ~ block <~ "}" ^^ {
      case id~from~to~block => FromTo(id, from, to, block)
    }
  def label            = (identifier <~ ":") ~ ("{" ~> block <~ "}" |
                                                instruction ^^ (x => Seq(x))) ^^ {case x~y => Label(x, y)}
  def break            = "break" ~> identifier ^^ Break

  def functionCall     =
    ("@" ~> identifier <~ "(") ~ repsep(valueParsers.expr, ",") <~ ")" ^^ {case x~y => FunctionInvocation(x, y)}
  def functionDef      =
    (("@" ~> identifier <~ "(") ~ repsep("$" ~> identifier, ",") <~ ")" <~ "{") ~ block <~ "}" ^^ {
      case id~param~block => (id, Function(param, block))
    }
  def letInBlock       = ("let" ~> functionDef.* <~ "in" <~ "{") ~ block <~ "}" ^^ {
    case fns~block => LetIn(fns.toMap, block)
  }
  def inlineFnDef      = "def" ~> functionDef ~ block ^^ {case x~y => LetIn(Seq(x).toMap, y)}

  def extInstruction: Parser[Instruction] = foreverBlock | ifLikeBlock | fromToBlock | label | break |
                                            letInBlock | inlineFnDef | functionCall
  def instruction   : Parser[Instruction] = basicInstruction | basicBlock | repeatBlock | extInstruction
  def block         : Parser[Block]       = instruction*

  def apply(s:String) = parseAll(block.*, s.replaceAll("//.*", "")) match {
    case Success(nodes, _)   => Left(nodes)
    case NoSuccess(err,next) => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}