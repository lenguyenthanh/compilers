# ulc (extended) grammar

## Syntax

```
t ::=                 term:
      | x             variable
      | λx. t         abstraction
      | t t           application
```

## Evaluation

# ulc (extended) grammar

## Syntax

```
term        ::=
              | x             variable
              | λx. t         abstraction
              | t t           application
```

## Evaluation

$$ {t_1 → t_1' \over t_1 t_2 → t_1' t_2} (E-App1)$$
$$ {t_2 → t_2' \over t_1 t_2 → t_1 t_2'} (E-App2)$$
$$ {(λx.t_12) t_2 → [x ↦ t_2]t_12} (E-AppAbs)$$
