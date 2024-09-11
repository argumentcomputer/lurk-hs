Build steps:

1. `cargo build`
2. `cabal build`

Test with

```
$ cabal repl
$ ghci> :set -Ltarget/debug -lplonk_verify
ghci> withCString "Rust" hello
Hello, Rust!
ghci> babybearinv 1
1
ghci> babybearinv 2
1006632961
ghci>
```