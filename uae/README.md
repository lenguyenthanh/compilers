# Untyped Arithmetic Expression

## Grammar
```
t ::=                                           terms:
      true                                      constant true
      false                                     constant false
      if t then t else t                        conditional
      0                                         constant zero
      succ t                                    successor
      pred t                                    predecessor
      iszero t                                  zero test
```

## Example

```
if false then 0 else 1
> 1
iszero (pred (succ 0))
> true
```

## TODO

- Lexer
- Scanner
- PrettyPrint

