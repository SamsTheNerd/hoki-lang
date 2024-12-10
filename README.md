# ><> Hoki ><>

Hoki is a pseudo stack-based, postfix ordered, repl based, functional programming language. It is also very much a work in progress.

The language is coded in haskell.

## Structure

We split development into 3 chunks, one for each person, these were: core language, frontend language, and translation between the two.

We decided on the core/frontend split so that evaluation/typing could be done on a smaller language closer to lambda calculus while still allowing for a rich and syntacticly sweet user-facing language.

### Core Language

The core language section focuses on evaluating and typing (inference and checking) of expressions. You can find the code for it in src/CoreLang. 

The core language itself is an extended lambda calculus, with some literals, type/data constructors, pattern matching cases, and letrec polymorphism. In the future we would like to have something similar to Haskell's typeclasses.

The evaluation is fairly straight forward, it lazily substitutes expressions as needed. There's not currently a way to force strictness. 

The type system is based on the bidirectional one given in *[Practical type inference for arbitrary-rank types](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/practical-type-inference-for-arbitraryrank-types/5339FB9DAB968768874D4C20FA6F8CB6)*, though ours does not support higher rank types. Ours does however support pattern matching and type/data constructors as well as using a (very limited at the moment) constraint gathering system rather than immediate solving.

Here's a runthrough of the modules:

- **CoreSorts** - defines our expression and type grammars along with some helpers, typeclass instances, and other definitions needed to prevent circular deps.
- **CoreEval** - handles expression evaluation.
- **Runad** - our runner monad. It handles error throwing and variable environment state.
- **CoreTyping** - handles expression typing.
- **Typad** - our type environment monad. Handles error throwing, variable environment state, constraint tracking, and some other type related helpers.
- **CoreLoader** - handles turning a raw program (list of statements) into an environment that execution and typing can be done in. Type checking/inference is dispatched here before anything else can happen. 

There are also some modules that are only really meant to be used internally for easier testing of the core language:

- **CoreParser** - extremely bad parser that parses a Haskell-like syntax for the core language.
    - can't parse variables that start with 'c'
    - sometimes function application needs parentheses, sometimes they don't?
    - data constructor definitions need arguments wrapped in parenthesis (ie `Pair (a) (b)` rather than `Pair a b`)
    - sometimes lines need semicolons sometimes they don't?
- **PrimIntegers** - integer primitives to be loaded in at core level.
- **CoreRepl** - a small repl environment for loading test files and evaluating/typing expressions.
- **CoreCompiler** - corelang is basically worse Haskell, so we can transpile from our core to haskell relatively trivially to get ourselves a free compiler! 
    - The semantics are a little different, notably we don't have call-by-need. 
    - This is mostly because I thought it'd be funny.

For some examples of core-lang tests, see resources/tests/core/. These can be loaded in the core-repl with `:l resources/tests/core/<filename>.clc`. 

### Frontend Hoki

***TODO FILL OUT***

### Hoki -> Core Translation

***TODO FILL OUT***

## Setup 

We use Cabal for managing our project. To install, simply clone/download the repo, making sure you have Cabal and GHC installed.

To run the Hoki repl:
```
> cabal run hoki-repl
```

To run the hoki-core-repl:
```
> cabal run core-repl
```

You can see the available repl commands by typing `:help`

You can also run:
```
> cabal install
```
to install `hoki-repl` and `hoki-core-repl` to your path. See [here](https://cabal.readthedocs.io/en/stable/how-to-package-haskell-code.html) for more details.

If you get an error about being unable to find goals, you may need to update your ghc `base` lib version. 


## Libraries Used

- **Parsec** for parsing.
- **Haskeline** for better user IO.
- **Containers, transformers, mtl** - I think these all come pre-installed anyways?

## Testing

We do not have as strict testing set up as I would have liked. For the core language we have a number of test files in resources/tests/core, but there isn't currently a mechanism for auto-running them.

## Future Work

### Core Lang
- Support typeclasses
- Further testing
- I'm not sure that type checking fully works?
- Better error handling 
    - move to structured errors rather than just strings
    - have traces
- Make core parser not horrid
- Finish haskell transpiler