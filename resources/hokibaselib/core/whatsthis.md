hoki has a number of operations that need to be implemented as a base lib, although not necessarily at the core level.

ideally these would be written in Hoki but we're out of time and need Something so they will simply be implemented with raw core haskell expressions
ie: (ELambda "a" (..))
These would be a pain to type by hand so i'm typing them in core lib, manually printing them to haskell expr with 
`CoreLang.CoreParser.getPFAsHs "filename" >>= putStrLn
and then modifying from there. This is not an ideal solution but it should work for now