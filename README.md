# TODO

A playground where I learn about interpreters, compilers, type theory and everything that related to those.

Please go to each directory of each example for more documentation. But in general, you can play around examples by using [scala-cli](https://scala-cli.virtuslab.org/). For example:

```bash
# run untyped lambda calculus REPL
scala-cli run ulc

# run test
scala-cli test ulc

# setup ide
scala-cli setup-ide ulc
```

The modules are sorted by its complexity:

- [Evaluation for untyped arithmetic expression](uae)
- [Interpreter for untyped lambda calculus](ulc)
