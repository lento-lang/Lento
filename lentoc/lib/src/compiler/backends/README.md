# Lento backends

Lento is designed to be a flexible and extensible compiler. It is intended to support multiple backends, which will allow it to generate code for different platforms and environments. The backends will be able to generate code in different formats, such as machine code, WebAssembly, or JavaScript, among others. Thus Lento will be able to target different platforms, such as native executables, web browsers, or Node.js, and thus provide a wide range of use cases and applications.

| Backend                            | Targets                              | Status         | Notes      |
| ---------------------------------- | ------------------------------------ | -------------- | ---------- |
| [Cranelift](cranelift/README.md)   | `Native`, `WebAssembly`              | 📖 Experimental |            |
| [LLVM](llvm/README.md)             | `Native`, `WebAssembly`              | 🛑 Not started  |            |
| [QBE](qbe/README.md)               | `Native`                             | 🛑 Not started  |            |
| [JavaScript](javascript/README.md) | `Browsers`, `Node.js`, `Deno`, `Bun` | 🛑 Not started  | Transpiler |
| [C](c/README.md)                   | `libc`, `POSIX`, `Windows`           | 🛑 Not started  | Transpiler |
| [Nasm](nasm/README.md)             | `x86`, `x86_64`, `ARM`, `ARM64`, etc | 🛑 Not started  |            |

The backends are implemented as separate modules, and able to be loaded by the Lento compiler.

> [!TIP] Contribute
> Any **contributions** to the development of the backends **are highly appreciated and welcome**!
> If you are interested in contributing, please refer to the [Contributing](../CONTRIBUTING.md) document for more information.

## Note on AOT and JIT

The current ambition is to **only** support **Ahead-Of-Time** (AOT) compilation. **Just-In-Time** (JIT) compilation may be considered in the future but is not a priority at the moment.
