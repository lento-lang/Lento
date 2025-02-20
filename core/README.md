<div align=center>
    <br>
    <img src="assets/logo_white.png" height=100px/>
    <h1>Lento <code>Core</code></h1>
    <p>
		<!-- portable to different platforms, interoperable languages, environments, etc. -->
		This shared library provides core functionality useful for; embedding Lento in other applications, creating custom tools, integrations, extensions, etc.
		Currently it's used in the <a href="https://github.com/lento-lang/Lento" target="_blank">Lento <code>CLI</code></a>.
	</p>
</div>

## Contents
This is the core implementation of the Lento programming language written in Rust. \
Including:
- Lexer and Parser
- AST pretty printer
- Type checker and inferencer
- Linter (static analysis)
- Standard library (See [Lento `Std`](src/stdlib))
	<!-- - Runtime
	- Garbage collector
	- Memory manager
	- Error handling -->
- Interpreter
	<!-- - Debugger support (planned)
	- JIT compiler (planned) -->
- Compiler <!-- (Ahead-of-time) -->
	<!-- - Optimizer
		- Dead code elimination
		- Constant folding
		- Constant propagation
		- Function Inlining
		- Pure function evaluation
		- Recursion elimination
		- Tail call optimization
		- Partial evaluation
		- Parallelization via Vectorization
			- SIMD
			- Multi-threading -->
	- Backends
		- Cranelift
			- Native: `x86_64`, `aarch64`, `s390x`, `riscv64`
				- Windows
				- macOS
				- Linux
			- WebAssembly <!-- : `wasm32`, `wasm64`, `wasm`, `wasi`, `wasi32`, `wasi64`, `wasiwasm`, `wasiwasm32`, `wasiwasm64` -->
	<!--
			- ~~LLVM~~ (planned)
			- ~~QBE~~ (planned)
			- ~~.NET~~ (planned)
			- ~~JVM~~ (planned)
			- ~~BEAM~~ (planned)
			- ~~MIR~~ (planned)
			- ~~MIPS~~ (planned)
		- Transpiler
			- ~~TinyCC~~ (planned)
			- ~~JavaScript~~ (planned)
				- ~~Browser~~ (planned)
				- ~~Node.js~~ (planned)
				- ~~Deno~~ (planned)
				- ~~Bun~~ (planned)
	-->
	<!-- - Static analysis
		- Data flow analysis
		- Control flow analysis
		- Abstract syntax tree analysis
		- Type analysis
		- Semantic analysis
		- Syntactic analysis
		- Lexical analysis
		- Side effect analysis (pure functions, immutability, etc.)
			- inference
			- checking
	- Code analysis
		- quality
		- smell
		- security
		- performance
		- duplication
		- complexity
	- Documentation generator
	- Test suite
	- Benchmark suite
	- Profiler
		- Memory profiler
		- CPU profiler
		- I/O profiler
		- Network profiler
		- Concurrency profiler
		- Threading profiler
		- Synchronization profiler -->
	<!-- - Code coverage -->
	<!-- - IDE support
		- Language server
		- Syntax highlighting
		- Code completion
		- Code navigation
		- Code refactoring
		- Code folding
		- Code lens
		- Code actions
		- Diagnostics
		- Hover
		- Signature help
		- Document symbols
		- Workspace symbols
		- References -->
	<!-- - Build system
		- Package manager
		- Dependency manager
		- Build tool
		- Task runner
	- Package manager
		- Registry
		- Repository
		- Index
		- Cache
		- Lock file
		- Manifest
		- Package
		- Version -->

## Requirements
- `rustc 1.78.0-nightly (46b180ec2 2024-03-08)` or later
