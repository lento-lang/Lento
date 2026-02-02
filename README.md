<div align=center>
    <br><br>
    <img src="assets/logo_white.png" height=250px/>
    <h1>The Lento Programming Language</h1>
    <em>
        A high-level functional programming language<br>
        designed to purify the object-oriented paradigm
    </em>
    <br/>
    <br/>
    <br/>
    <img alt="Github build passinig" src="https://img.shields.io/badge/build-passing-brightgreen">
    <img alt="GitHub release (latest by date)" src="https://img.shields.io/github/v/release/Lento-Lang/Lento?style=flat-square">
    <img alt="GitHub contributors" src="https://img.shields.io/github/contributors/Lento-lang/Lento?style=flat-square">
</div>

> [!WARNING] Work in Progress
> We appreciate your interest in Lento!
> However, please be aware that Lento is currently in the early stages of development and **nowhere near production-ready**.

<br/>
<br/>

# Welcome

Lento is an open-source, strongly typed and lazy evaluated programming language with aim to purify the object-oriented paradigm.
Types provide a way to describe the shape of an object, providing better documentation, and allowing Lento to validate that your code is working correctly.
This project provides a helpful command line interface tool for the Lento programming language.

> Learn more about Lento at http://lento-lang.org.

<br/>

## Installation 📦

The Lento toolchain is available for x86/64 Windows, Linux and macOS.
You can install the latest version using the commands below.
Alternatively, download and install the latest version from the [releases page](https://github.com/lento-lang/Lento/releases).

### Windows

```powershell
> iwr "https://raw.githubusercontent.com/lento-lang/Lento/main/install.ps1" -useb | iex
```

### Linux and macOS

```bash
$ curl -sSL "https://raw.githubusercontent.com/Lento-lang/Lento/main/install.sh" | bash
```

<br>

## Documentation 📗

Learn more about the Lento programming language by visiting the links below.
A good tip is to start playing around with the examples!

- [Get Started!](https://lento-lang.org/docs/#getting-started)
- [Code examples](https://github.com/lento-lang/Lento/tree/main/examples)
- [Language Specification](https://lento-lang.org/docs/language_specification_v1.pdf)

<!--* #### [Project euler solutions](doc/PROJECT_EULER.md)-->

## The Team 👨‍💼

Lento is developed by a driven team working hard to create as good as possible experience for all of you developers. [Meet us here! 🙋‍](https://lento-lang.org/team)

Do you want to help the project? Maybe even get your own testimonial on the official language website?
Then consider contributing and become a member with magical powers 🧙‍♂️!

## Contribute 🌟

Lento is truly a community effort, and we welcome contribution from hobbyists and production users, from newcomers and seasoned professionals. Come help us make the Lento experience even better!
Start by reading the (1) **[contributing guidelines](CONTRIBUTING.md)**.

**Get started** by pulling the main branch and start fiddling around.<br/>
When you feel confident enough, take a look at the (2) **[good first issues](https://github.com/Lento-lang/Lento/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22)**.
Any maybe your code will be used by thousands of developers around the world! 💪🎉🎊

### Development 🛠️

This repository contains the `core` language implementation and a `cli` command line interface toolchain for the Lento programming language.

Run the following command to start the REPL in debug mode, providing full error backtraces:

```powershell
$env:RUST_BACKTRACE='full'; cargo run -q -- repl -t
```

Always verify all tests pass before pushing your code:

```bash
cargo test
```

> Currently lots of tests are failing, so don't worry if you see a lot of red text. We are working on it! 🚧

## License 📜

Lento is licensed under the [MIT License](LICENSE).
