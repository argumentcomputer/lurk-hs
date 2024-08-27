Build steps:

1. `nix develop` (if needed)
2. `cargo build`
3. `cabal build`

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

Note that `nix build` is broken and will error with

```
warning: Git tree '/home/jcb/plonk-verify' is dirty
error: builder for '/nix/store/lz6zm8mfif4bh7wsai8dx6blpzymrvz1-plonk-verify-deps-0.1.0.drv' failed with exit code 101;
       last 10 log lines:
       >
       > Caused by:
       >   failed to parse manifest at `/nix/store/nix47gay33f6mqmrp3xb77189llkfcil-git-dependencies/sphinx-recursion-program-1.0.0-dev/Cargo.toml`
       >
       > Caused by:
       >   error inheriting `edition` from workspace root manifest's `workspace.package.edition`
       >
       > Caused by:
       >   failed to find a workspace root
       > [naersk] cargo returned with exit code 101, exiting
       For full logs, run 'nix log /nix/store/lz6zm8mfif4bh7wsai8dx6blpzymrvz1-plonk-verify-deps-0.1.0.drv'.
error: 1 dependencies of derivation '/nix/store/dazvfzn454x9x93gxr0fpdg93m3vv7di-plonk-verify-0.1.0.drv' failed to build
```

This is likely a naersk workspace inheritance issue related to:

- https://github.com/nix-community/naersk/issues/318
- https://github.com/nix-community/crate2nix/issues/311
- https://github.com/ipetkov/crane/pull/224
- https://github.com/nix-community/naersk/issues/310

