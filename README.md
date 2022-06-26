# README

A playground where I learn about interpreters, compilers, type theory and everything that related to those.

In general, you can play around examples by using [scala-cli](https://scala-cli.virtuslab.org/). For example:

```bash
# run untyped lambda calculus REPL
scala-cli run ulc

# run test
scala-cli test ulc

# setup ide
scala-cli setup-ide ulc

# build native package with GraalVM
scala-cli package --native-image ulc  -o bin/ulc -f
```

Please go to each directory of each example for more details documentation:

- [Evaluation for untyped arithmetic expression](uae)
- [Interpreter for untyped lambda calculus](ulc)
