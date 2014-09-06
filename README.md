JoustExt
========

JoustExt is a simple language compiling to BF Joust.
It adds a function call system (evaluated at compile time) and a continuation system for constructs like if/else.

To compile the compiler, you need sbt installed.
Run `scripts/make` or `sbt package`.
To compile a JoustExt program as follows, run `scripts/jxc foo.jx foo.bf`.
To compile the example programs in examples/*, run `scripts/jxc`.

Basic Instructions
------------------

All BFJoust commands and constructs except `(a{b}c)*n` can be directly embedded in JoustExt code.
Unlike BF Joust, however, comments have to be marked with `//` like in C.

    ++++ // This is a comment
    [+]  // If we didn't have the '//', it would be a syntax error!
    (-)*30(---)*-1

If needed for some reason, any command or block can be suffixed with an `;` to disambiguate.

Variables and Functions
-----------------------

Functions names are prefixed with `@` and variable names are prefixed with `$`.
Variables and function names all must match the pattern `[a-zA-Z_][a-zA-Z0-9_]`.

    // Declare a function named fn, with two parameters a and b.
    @fn($a, $b) {
      $c = ($a + $b) * $a + $b
      ++++
    }
    @fn(1, 2)
    @fn(2, 3)

Function calls are compiled by splicing.
Thus, the function call `@fn(30, 40)` would be the same as the following code:

    local { // local creates a new scope.
      $a = 30
      $b = 40
      $c = ($a + $b) * $a + $b
      ++++
    }

Expressions
-----------

In BF Joust expressions, only five operators exist: 
addition (`+`), 
subtraction/unary negation (`-`), 
multiplication (`*`), 
division (`/`),
and modulo (`%`).
Unary negation has the highest precedence,
multiplication, division, and modulo share the next lowest precedence,
and addition and subtraction share the lowest precedence.

You may use expressions in the `(a)*n` construct directly, such as in `(+)*$a` or `(+)*($a + $b)`.
The parentheses are required for any expression more complicated than a constant value or a single variable.

Scope
-----

For ease of implementation, variables and functions in JoustExt are dynamically scoped
However, as changes to variables are limited to the block they are modified in, variables are effectively immutable despite the dynamic scoping.

    @fn() {
      // Despite not being available in lexical scope, $a can still be accessed here.
      (+)*$a
      @fn2() // And since @fn2 will be defined by the time @fn is called, it is accessible here.
    }
    @fn2() {
      (-)*$a
    }

    $a = 1
    local {
      $a = 2
      // $a is 2 here.
      @fn() // and this evaluates to (+)*2 (-)*2
    }
    // but $a is 1 here!
    @fn() // so this evaluates to (+)*1 (-)*1

Predicates and Conditionals
---------------------------

Conditionals are evaluated at compile time, just like functions.
If predicate evaluates to true, the contents are included, otherwise the else block is included if it exists.

    if($a > 0) {
      (+)*$a
    } else {
      (-)*(-$a)
    }

    if ($b == 0 || $c == 0) {
      (<)*1000 // byeeee~
    }

Valid comparison operators are
less than (`<`),
greater than (`>`),
less than or equal to (`<=`),
greater than or equal to (`>=`),
equals (`==`),
and not equals (`!=`).
Valid boolean operators are
and (`&`),
or (`|`),
and not (`!`).
They have no particular order of precedence, so use parentheses liberally.

Miscellaneous features
----------------------

You may include raw text in a JoustExt program by writing `raw` followed by a quoted string.
These quoted strings are much simpler than in most languages.
There is no escape mechanism, and new lines/etc can be directly embedded.
However, this means that there is absolutely no way to output a raw `"` character.

The `raw` may be followed by `+margins` token to cause the JoustExt compiler to strip all text up to the first `|` on each line.
In other words, Scala's String.stripMargins is called on the contents of the block.

The `abort` command has the same syntax as the `raw` command, with the exception that `raw` doesn't support `+margins`.
`abort` will output a message to the output file, and then execute `(.)*-1`.

`//` comments in raw blocks are **not** output in the final program, due to a lazy implementation of comments.

    raw "foo"
    raw "
         foo"
    raw "\\..\"
    raw +margins "| foo
                  | bar"
    raw " foo // baz
          bar" // baz will NOT appear in the final output.
    abort "We can't do anything anymore. :("

There is a `for` block construct that allows you to count iterations unlike `(a)*n`.

    for($i in $a + $b to $c) {
      +++
    }
    for($i in 9 to 30) {
      [-] (<)*$i (+)*100 (>)*$i
    }

Finally, you may use the `terminate` instruction at any time to stop execution at the `linearize` phase.
Sometime in the future, it'll work in the `exprs` phase too.

Continuations
-------------

For some advanced control flow, JoustExt supports an continuation system.

    callcc(@a) {
      ++
      [ @a() ]
    }

`callcc(@a)` creates a special function named `@a`, which when called, jumps to whatever is after the `callcc` block.
This is done by code rewriting which is very prone to generating very long code, however, so, use this with care.
To prevent that, `reset` blocks may be used to contain the scope of a continuation.
When a continuation is generated, and a `reset` block is encountered, the program will halt.
