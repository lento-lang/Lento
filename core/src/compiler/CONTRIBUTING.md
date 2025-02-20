# Contributing to the Lento compiler

> [!NOTE]
> **Hello friend!** \
> We're glad to see you're interested in helping us improve the Lento compiler.
> Let's hack together and make Lento the best it can be!

The Lento compiler is an open-source project, and contributions are highly appreciated and welcome. If you are interested in contributing, please refer to the following sections for more information.
Here are some ways you can contribute to the development of the Lento compiler:

- [Contributing to the Lento compiler](#contributing-to-the-lento-compiler)
	- [Reporting issues](#reporting-issues)
	- [Contributing documentation](#contributing-documentation)
	- [Contributing code](#contributing-code)
	- [Contributing to the backends](#contributing-to-the-backends)

## Reporting issues

Any issues, bugs, or feature requests can be reported in the [Issues](https://github.com/lento-lang/Lento-Core/issues/new) section of the Lento-Core repository.

## Contributing documentation

The documentation is an important part of the Lento project. It is intended to be a comprehensive and up-to-date resource for users and contributors. If you are interested in contributing to the documentation, please refer to the [Documentation](../../docs/README.md) section for more information.

## Contributing code

The Lento compiler is written in the Rust programming language. If you are interested in contributing to the development of the compiler, the following steps will help you get started:

1. Fork the [Lento-Core](https://github.com/lento-lang/Lento-Core) repository on GitHub.
2. Clone your fork of the repository to your local machine.
3. Create a new branch for your intended changes.
4. Make your changes to the code.
5. Run the tests to ensure that your changes do not introduce any regressions.
   1. If you are adding new functionality, please add tests to cover the new code.
   2. Run the tests with `cargo test`.
6. Commit your changes and push them to your fork on GitHub.
7. Create a pull request from your branch to the `develop` branch of the Lento-Core repository.
8. Wait for the maintainers to review your pull request and provide feedback.
   1. Address any feedback and make any necessary changes to your pull request.
9. If your pull request is approved, it will be merged into the `develop` branch and will be included in the next release of Lento! 🎉

## Contributing to the backends

Lento is designed to be a flexible and extensible compiler. It is intended to support multiple backends, which will allow it to generate code for different platforms and environments. The backends will be able to generate code in different formats, such as machine code, WebAssembly, or JavaScript, among others. Thus Lento will be able to target different platforms, such as native executables, web browsers, or Node.js, and thus provide a wide range of use cases and applications.
