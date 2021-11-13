# racket-lox

racket-lox is an implementation of [the Lox language](https://github.com/munificent/craftinginterpreters/) from Robert Nystrom's book [Crafting Interpreters](https://craftinginterpreters.com/) in Typed Racket.

This interpreter is based on the tree-walking Java interpreter described in part II of the book, meaning it largely follows the book's imperative style, rather than the functional programming that is more common in Racket. However, there are a few differences in the implementation:

- Rather than using classes and inheritance to model the AST nodes, we instead model the tree as a union type of structs. This means that we use pattern matching rather than [the Visitor pattern](https://craftinginterpreters.com/representing-code.html#the-visitor-pattern) in the evaluator and the resolver.
- As such, there is no need for [the metaprogramming described in chapter 5](https://craftinginterpreters.com/representing-code.html#metaprogramming-the-trees) to create classes for the AST nodes.
- `Callable` is implemented as a union type of `Function`, `NativeFunction` and `Class`. Because of annoyances surrounding cyclic imports, the `Callable`-related code lives in [interpreter.rkt](src/interpreter.rkt) rather than in a separate file.
- Error related functions are placed in a separate file, [error.rkt](src/error.rkt) rather than with the top-level program interface, which lives in [main.rkt](src/main.rkt).

## Running the interpreter

To run the interpreter, simply use `racket src/main.rkt`, passing a `.lox` file as an argument to run a script.