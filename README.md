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
Make sure you have Rust installed by following the instructions [here](https://rustup.rs/).

## Logging Configuration

The verifier uses two logging systems:

- Rust logging via `tokio-tracing`, configured through the `RUST_LOG` environment variable. See the [tracing documentation](https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html) for detailed configuration options.
- Go logging for FFI calls, configured through the `SP1_GO_LOG` environment variable.

To set a global log level (e.g. warn), configure both variables:

```bash
RUST_LOG=warn SP1_GO_LOG=warn ./plonk-verify ...
```

Valid log levels are: error, warn, info, debug, trace

### 3. Install Haskell (Cabal)
Ensure that you have Haskell and Cabal installed by following the instructions [here](https://www.haskell.org/cabal/).

## Build steps
- Build the Rust part of the project:
```bash
cargo build --release
```

- (For macOS only/cabal repl only) Build the dynamic library:
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

## Troubleshooting

If you get "error while loading shared libraries", try:
```bash
cargo clean
```

This happens if the library is built against the `.so` file, and can happen if you ran `cargo rustc --crate-type cdylib` in the past.
