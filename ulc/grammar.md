# ulc (extended) grammar

## Syntax

### term

```
t ::=                              term:
      | x                          variable
      | λx. t                      abstraction
      | t t                        application
      | (t)                        grouping
```

### statement

```
statement ::=                      statement:
               x = term            assignment
```

### comment

```
comment ::=                        comment:
            -- string              single line comment
```

Note: Only support comment at start of a line.

### program

```
program ::=                        program:
            | term                 term
            | statement            statement
            | comment              comment
```
