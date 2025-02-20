# Lento `Cranelift` backend

> [!WARNING]
> This backend is under experimental development and is not yet ready for use.

The `Cranelift` backend is a work in progress. It is intended to generate Cranelift IR from the Lento IR.
The generated Cranelift IR will be able to be compiled into machine code and run on any platform that Cranelift supports.

The Cranelift project is a low-level, retargetable code generator developed by Bytecode Alliance. It is designed to be used as a backend for programming languages, and is used by WebAssembly runtimes such as Wasmtime and Lucet.

The Cranelift project is available at [Cranelift](https://cranelift.dev/).

> [!TIP]
> Any **contributions** to the development of the backend **are highly appreciated and welcome**!
> If you are interested in contributing, please refer to the [Contributing](../../CONTRIBUTING.md) document for more information.
