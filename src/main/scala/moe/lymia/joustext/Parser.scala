package moe.lymia.joustext

import ast._, astextension._

object Parser extends scala.util.parsing.combinator.RegexParsers {
  def basicInstruction = ("+" ^^^ IncMem) |
                         ("-" ^^^ DecMem) |
                         (">" ^^^ IncPtr) |
                         ("<" ^^^ DecPtr)

  def block: Parser[Block] = ???

  def apply(s:String) = parseAll(block.*, s.replaceAll(";.*", "")) match {
    case Success(nodes, _)   => Left(nodes)
    case NoSuccess(err,next) => Right("At line "+next.pos.line+", column "+next.pos.column+": "+err)
  }
}