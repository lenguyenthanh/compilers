# Untyped Lambda Calculus

An interpreter for [untyped lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (ulc) with some extended grammars implemented in Scala 3. I added some extended syntax to make it easier to use (for example let binding or \ can be used instead of λ). You can check the formal grammar definition [here](./grammar.md)

## Features

- REPL
- Load programs from files
- [Example](./examples.ulc) with boolean, number, Y combinator and factorial.

![Repl example](./examples.gif)

## Architecture Overview

The overall architecture of this interpreter looks like:
```
       Lexer          Parser              de Bruijn indices transformation                  Evaluator
String =====> [Token] ======> Parsed AST ==================================> nameless AST  ===========> result
```

### Lexer

First we need a [Lexer](https://en.wikipedia.org/wiki/Lexical_analysis) (or Scanner, Tokenizer) to parse the program into a list of [Token](https://en.wikipedia.org/wiki/Lexical_analysis#Token). I use [Parser Combinators](https://en.wikipedia.org/wiki/Parser_combinator) technique with help of the wonderful [cats-parse](https://github.com/typelevel/cats-parse) library.

### Parser

Next step is a [Parser](https://en.wikipedia.org/wiki/Parsing), which receives a list of Token and turn it into a [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) or parsed tree or AST for short. I also using Parser Combinator technique here by building a simple and stupid [Parser Combinator](./Parser.scala) by my own.

Here is the parsed AST code:

```Scala
case class Stmt(val info: Info, val name: String, term: Term)
enum Term(val info: Info):
  case Var(override val info: Info, val name: String)             extends Term(info)
  case Abs(override val info: Info, val arg: Var, val term: Term) extends Term(info)
  case App(override val info: Info, val t1: Term, val t2: Term)   extends Term(info)
```

Note: `val info: Info` here is stored location information of each note, which can be very helpful when we need to show errors to users (which I haven't done, but I should).

### de Bruijn Index Transformation

[de Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index) or nameless representation of terms. Quote from Type and Programming Languages book:

```
De Bruijn’s idea was that we can represent terms more straightforwardly — if less readably — by making variable occurrences point directly to their binders, rather than referring to them by name. This can be accomplished by replacing named variables by natural numbers, where the number k stands for “the variable bound by the k'th enclosing λ.” For example, the ordinary term λx.x corresponds to the nameless term λ.0, while λx.λy. x (y x) corresponds to λ.λ. 1 (0 1). Nameless terms are also sometimes called de Bruijn terms, and the numeric variables in them are called de Bruijn indices.
```

Our nameless AST is presented as below:

```
enum BTerm:
  case BVar(val index: Int)
  case BAbs(val term: BTerm)
  case BApp(val t1: BTerm, val t2: BTerm)
```

Notice that `BTerm` and `Term` look almost the same (without name variable and location information).

### Evaluation

The final step is the evaluation. I use the exact implementation that is described [in the Evaluation section here](https://crypto.stanford.edu/~blynn/lambda/).

We show the final result in de Bruijn form, which is not easy to read. I can improve it later by coloring the output (A variable occurrence and its binding site are assigned the same color, so that a 228 reader no longer has to count binding sites).

## Resources

### Lambda Calculus

- [A Tutorial Introduction to the Lambda Calculus](https://personal.utdallas.edu/~gupta/courses/apl/lambda.pdf)
- [History of Lambda-calculus and Combinatory Logic](http://www.users.waitrose.com/~hindley/SomePapers_PDFs/2006CarHin,HistlamRp.pdf)

### Implementation

- [Chapter 5,6, 7 in Type and Programming Languages book](https://www.cis.upenn.edu/~bcpierce/tapl/)
- [Ben Lynn's lambda calculus website](https://crypto.stanford.edu/~blynn/lambda/)

## TODO

- Coloring the output
- Better error messages
- Recursion scheme
- Fully functional for repl/main
