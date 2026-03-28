## VS Code Configuration

There are some options for tablegen-lsp.

### Example

```jsonc
{
    // The path to the include directory.
    // Type: [string]
    "tablegen-lsp.includePath": [
        "/path/to/include",
    ],

    // Defines a source root `.td` file used at startup. If you manually set the source root using the `Set Source Root` menu, this configuration is ignored.
    // Type: string
    "tablegen-lsp.defaultSourceRootPath": "/path/to/root.td"
}
```
