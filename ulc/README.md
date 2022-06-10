# Untyped Lambda Calculus Interpreter

This an untyped lambda calculus (ulc) interpreter implement in Scala 3.

## Introduction

The core part (evaluation) is a direct copy/paste from chapter 9 of the book [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) of Benjamin C. Pierce.

## Todo

- [ ] Support Assignment operator
- [ ] ParsedAST to de Bruijn's indices AST
- [ ] REPL
- [ ] Update README
  - grammar
  - operational semantic rules
  - Implementation
  - setup/run/tests
- [ ] Some more tests
- [ ] Load program from files

## Issues with free variables

## variable lookup rule order

- context
- env
- free

## Overview

        Lexer            Parser            Name removing (de Bruijn indices)                  Evaluator           Pretty printer
String ======> [Token] ======> Parsed AST ===================================> nameless AST  ===========> result ================> String
