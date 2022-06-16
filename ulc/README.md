# Untyped Lambda Calculus Interpreter

An interpreter for untyped lambda calculus (ulc) with some extended grammars implemented in Scala 3.

## Introduction

The core part (evaluation) is a direct copy/paste from chapter 9 of the book [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) of Benjamin C. Pierce.

## Todo

- [x] Support Assignment operator
- [x] ParsedAST to de Bruijn's indices AST
- [x] REPL
- [ ] Update README
  - grammar
    - operational semantic rules
    - Implementation
    - setup/run/tests
- [x] Some more tests
- [x] Load program from files
- [x] Support single line comment
- [ ] Resource

## Issues with free variables

## variable lookup rule order

- context
- env
- free

## Overview

        Lexer            Parser            Name removing (de Bruijn indices)                  Evaluator           Pretty printer
String ======> [Token] ======> Parsed AST ===================================> nameless AST  ===========> result ================> String
