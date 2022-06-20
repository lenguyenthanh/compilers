# Untyped Lambda Calculus

An interpreter for [untyped lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) (ulc) with some extended grammars implemented in Scala 3. I added some extended syntax to make it easier to use (like let binding or \ can be used instead of λ). You can check the formal grammar definition [here](./grammar.md)

## Features

- repl
- load programs from files
- factorial
- update examples

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

### de Bruijn Index Transformation

[de Bruijn index](https://en.wikipedia.org/wiki/De_Bruijn_index) or nameless representation of terms.

### Evaluation

The final step is the evaluation. I use the exact implementation that is described [in the Evaluation section here](https://crypto.stanford.edu/~blynn/lambda/).

## Resources

- [Chapter 5,6, 7 of Type and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)
- [Ben Lynn's lambd](https://crypto.stanford.edu/~blynn/lambda/)
