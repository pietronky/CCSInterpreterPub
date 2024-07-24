## Some useful documentation

- [Alex documentation](https://haskell-alex.readthedocs.io/en/latest/)
- [Happy documentation](https://haskell-happy.readthedocs.io/en/latest/)
- [An example of synergy between Alex and Happy](https://github.com/da-x/happy-alex-example/)
- [Reactive Systems - Modelling, Specification and Verification](https://rsbook.cs.aau.dk/)

## CCSVP code syntax (as a Backus-Naur grammar)

### Tokens
```
*capitalized* = [A-Z][_a-z0-9]*

*channel* = *lowercase*

*constant* = *capitalized*

*enum_value* = *capitalized*

*inaction* = 0

*integer* = 0|\-?[1-9][0-9]*

*lowercase* = [a-z][_a-z0-9]*

*new_channel* = *channel*

*old_channel* = *channel*

*type* = Enum|Nat

*variable* = *lowercase*
```

### Syntax
```
<source_code> ::=  {-# OPTIONS_CCS -nat-max *integer* #-} <declarations>

<action> ::=  *channel*
              | \overline{ *channel* }
              | *channel* ( *variable* : *type* )
              | \overline{ *channel* } ( <expression> )
              | \tau

<channels> ::=  *channel*
                | \overline{ *channel* }
                | *channel* , <channels>
                | \overline{ *channel* } , <channels>

<declarations> ::=  *capitalized* = <process> ;
                    | *capitalized* ( <params> ) = <process> ;
                    | *capitalized* = <process> ; <declarations>
                    | *capitalized* ( <params> ) = <process> ; <declarations>

<expression> ::=  *enum_value*
                  | *variable*
                  | *integer*
                  | <expression> + <expression>
                  | <expression> \cdot <expression>
                  | <expression> / <expression>
                  | ( <expression> )

<expressions> ::= <expression>
                  | <expression> , <expressions>

<guard> ::= <expression> = <expression>
            | <expression> < <expression>
            | \neg{ <guard> }
            | <guard> \land <guard>
            | ( <guard> )

<label_changes> ::= *new_channel* / *old_channel*
                    | *new_channel* / *old_channel* , <label_changes>

<params> ::=  *variable* : *type*
              | *variable* : *type* , <params>

<process> ::= <action> . <process>
              | <process> + <process>
              | if <guard> then <process>
              | *constant*
              | *constant* ( <expressions> )
              | *inaction*
              | <process> | <process>
              | <process> \ { <channels> }
              | <process> [ <label_changes> ]
              | ( <process> )
```

## Caveat
### Non strictly typed channels

   > For example, something like `in(x : Nat)`
   > means "Accept only `Nat` typed values on channel `in`
   > and discard all the other values"

### Shadowing

   > For example in the following snippet `x` is shadowed in the second choice of `Foo`
   > ```
   > Main = Foo(5);
   > Foo(x : Nat) = \overline{out}(x + 5).0 + in(x : Nat).Bar(x + 10);
   > Bar(x : Nat) = \overline{out}(x + 15).0;
   > ```

### Precedence rules

In decreasing binding strength: restriction and relabeling, action prefixing, parallel composition and summation.

> **Note:** Follow standard precedence rules for arithmetic expressions

### About `Main`

1. Declarations **must** include `Main` in order for code to be executed.
2. `Main` **must** have no signature.

## Running CCSVP source code

```
cabal run ccs-interpreter -- <file>
```
