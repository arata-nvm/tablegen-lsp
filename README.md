# tablegen-lsp

A language server and VS Code extension for [TableGen](https://llvm.org/docs/TableGen/index.html).

## Installation

This extension is available on the [VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=arata-nvm.tablegen-lsp).

## Configuration

See [`docs/configuration.md`](docs/configuration.md) for configuration options.

## Features

See [`docs/features.md`](docs/features.md) for a list of features currently implemented or planned.

## Development

### Building from Source

```bash
# Clone the repository
git clone https://github.com/arata-nvm/tablegen-lsp
cd tablegen-lsp

# Build the project
cargo build
```

### Running Tests

```bash
cargo test
```

### Debugging

1. Open the [`vscode`](vscode) directory in VS Code
2. Press <kbd>F5</kbd> to start debugging

## License

This project is licensed under the [MIT License](vscode/LICENSE).

Some files in this project are derived from the LLVM Project and are licensed under the Apache License v2.0 with LLVM Exceptions:
- [`vscode/third-party/language-configuration.json`](vscode/third-party/language-configuration.json)
- [`vscode/third-party/language-grammar.json`](vscode/third-party/language-grammar.json)

For the full text of the Apache License v2.0 with LLVM Exceptions, please see [`vscode/third-party/LICENSE.TXT`](vscode/third-party/LICENSE.TXT).
