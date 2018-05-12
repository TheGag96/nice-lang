# nice-lang

This is a parser/interpreter for a small language I wrote for an undergraduate study thing I did with my professor in lieu of a Compilers course. I can't think of a good name for it at the moment, so I chose a play on cool-lang. It's meant to be incredibly simple for the purposes of teaching basic computation with computers.

## Compiling

```sh
dmd parser.d lexer.d   # use ldc2 -O instead for SPEED
```

## Philosophy

As mentioned, this language is meant to be super duper simple, inspired mainly by Python, Pascal, and C. The hope is that it can be used to teach beginners how to solve problems with the basic building blocks of programs through a language that could be taught in a day or two. This way, they're not wasting time trying to learn how to make a compiler for a complex language happy. A couple key things to note:

* Dynamic typing, but functions can optionally be type-checked
* Multi-line keywords have matching ending keywords (e.g. `if` and `endif`)
* `=` for equality, `:=` for assingment. No `==`!
* New types are simply named tuples, and nothing else. It can be nice to wrap up a couple values with names, but OO is too centralizing and distracts from the goal of the language.

## Features

Implemented:

* Variables
* `if` statements
* `while` statements
* `for` statements over integer ranges
* Functions and `return` statements
* Dynamic typing with integers, floats, strings, characters, functions

Planned:

* Arrays
* Defined types / named tuples
* Type checking on functions
* Built-in functions like `print`

## Known issues

* `return` is supposed to be allowed only at the end, but you can get around it very easily (and it does nothing in those cases IIRC)
* There is no type checking on functions despite it aleady being parsed
* Strings and characters cannot use escape sequences