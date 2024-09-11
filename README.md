# Build and Test Instructions

## Prerequisites

Before building and testing the project, make sure you have the following installed:

### 1. Install Go
If you don't already have Go installed, you can install it using the following commands:

```bash
# For Linux and macOS:
wget https://go.dev/dl/go1.21.1.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.1.linux-amd64.tar.gz
export PATH=$PATH:/usr/local/go/bin

# For macOS using Homebrew:
brew install go

# For Windows:
Download the installer from https://go.dev/dl/ and follow the installation instructions.
```

### 2. Install Rust
Make sure you have Rust installed by following the instructions [here](https://rustup.rs/)..

### 3. Install Haskell (Cabal)
Ensure that you have Haskell and Cabal installed by following the instructions [here](https://www.haskell.org/cabal/).

## Build steps
- Build the Rust part of the project:
```bash
cargo build --release
```

- (For macOS only) Build the dynamic library:
```bash
cargo rustc --release --crate-type cdylib
```

- Build the Haskell part of the project:
```bash
cabal build
```

## Testing
To run tests for the Rust components, execute:

```bash
cargo test
```

To run the example Haskell application, execute:
```bash
cabal run test
```

If that fails with "error while loading shared libraries", try instead: (for using `target/release/libplonk_verify.so`)
```bash
LD_LIBRARY_PATH=$(pwd)/target/release cabal run test
```
