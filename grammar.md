# tlc grammar

## Type Rules

### Definitions

```
T ::=                               types
      | Bool                        type of booleans
      | T1 -> T2                    type of functions
```

`->` is a `type constructor`, which is right-associate - that is, the type expression `T1 -> T2 -> T3` means `T1 -> (T2 -> T3)`

### The Typing Relations

T-Abs: `Γ, x: T1 && t2: T2 => Γ, λx: T1. t2 : T1 -> T2`
T-Var: `x: T ∈ Γ => Γ, x: T`
T-App: `Γ, t1: T11 -> T12 && Γ, t2: T11 => Γ, t1 t2: T12`
T-Iff: `Γ, t1: Bool && Γ, t2: T && Γ, t3: T => Γ, if t1 then t2 else t3: T`

## Syntax

```

```
