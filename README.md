# Kilo-Scala

[![Build Status](https://github.com/miueon/kilo-scala/actions/workflows/ci.yml/badge.svg)](https://github.com/miueon/kilo-scala/actions/workflows/ci.yml)

Kilo-Scala is a lightweight text editor written in Scala 3 and compiled to a native executable using Scala Native. It is a port of the original [Kilo editor](https://viewsourcecode.org/snaptoken/kilo/index.html) and is inspired by the functional programming principles of the Typelevel ecosystem.

## Features

*   **Cross-platform:** Runs on any platform that supports Scala Native.
*   **Small and fast:** Compiles to a small, native executable with a fast startup time.
*   **Basic text editing:** Insert and delete characters, move the cursor, and more.
*   **File I/O:** Open and save files.
*   **Syntax highlighting:** Supports syntax highlighting for various languages.
*   **Search:** Search for text within a file.

## Technology Stack

*   [**Scala 3.7.1**](https://docs.scala-lang.org/scala3/): Modern, multi-paradigm programming language with latest LTS version.
*   [**Scala Native**](https://scala-native.org/): Compiles Scala code to native executables with C FFI support.
*   [**Cats 2.12.0**](https://typelevel.org/cats/): Functional programming abstractions and type classes.
*   [**Cats Effect 3.7.0-RC1**](https://typelevel.org/cats-effect/): Pure asynchronous runtime with fiber-based concurrency.
*   [**Cats MTL 1.5.0**](https://github.com/typelevel/cats-mtl): Monad transformer library for state management.
*   [**OS-Lib 0.10.2**](https://github.com/com-lihaoyi/os-lib): Cross-platform file system operations.
*   [**Bindgen Plugin**](https://github.com/indoorvivants/sn-bindgen): Automated C header bindings for Scala Native.
*   [**MUnit 1.0.0**](https://scalameta.org/munit/): Lightweight testing framework.
*   [**sbt**](https://www.scala-sbt.org/): The interactive build tool for Scala.

## Design and Architecture

Kilo-Scala demonstrates modern functional programming patterns using the latest Typelevel ecosystem libraries. The architecture emphasizes type safety, composability, and resource management.

*   **Purely Functional:** Built using the `IO` monad from Cats Effect 3.7, ensuring all side effects are properly managed and composed. The application embraces pure functional programming principles throughout.
*   **Fiber-Based Concurrency:** Uses Cats Effect's lightweight fibers for concurrent operations instead of OS threads. The event loop runs keyboard input and signal handling as separate fibers with proper cancellation support.
*   **State Management:** Editor state is managed using `StateT[IO, EditorConfig, *]` monad transformer with Cats MTL's `Stateful` type class for clean state operations.
*   **Event-Driven Architecture:** Asynchronous event processing using `cats.effect.std.Queue` for type-safe, backpressured event handling. Window resize signals and keyboard input are processed concurrently via `Dispatcher`.
*   **C FFI with Bindgen:** Automated C header bindings using the bindgen plugin for terminal raw mode operations. Direct integration with POSIX APIs for terminal control.
*   **Modern Scala 3 Features:** Leverages opaque types, enums, extension methods, and indentation-based syntax for clean, type-safe code.
*   **Resource Safety:** Comprehensive resource management using `Resource` data type ensures proper cleanup of terminal state, file handles, and fiber lifecycles.

## Project Structure

```
src/main/scala/
├── Main.scala              # Application entry point with IOApp
├── domain/                 # Core data types and business logic
│   ├── EditorConfig.scala  # Editor state and configuration
│   ├── Row.scala          # Text row representation
│   ├── SyntaxConfig.scala # Syntax highlighting configuration
│   └── constants.scala    # Application constants and key bindings
├── effect/                # Effect type classes and utilities
│   └── LiftIO.scala       # Type class for lifting IO operations
├── macro/                 # Compile-time macros
│   └── escStr.scala       # String escape sequence macros
├── par/                   # Parallel processing and event handling
│   ├── Event.scala        # Event types (Key, WindowResize, Quit)
│   └── EventLoop.scala    # Fiber-based event loop with signal handling
├── rawmode/               # Terminal raw mode operations
│   └── TermIOS.scala      # C FFI for terminal control
├── services/              # Business logic and operations
│   ├── DrawOps.scala      # Screen drawing and rendering
│   ├── EditorOps.scala    # Core editor operations
│   ├── KeyOps.scala       # Keyboard input handling
│   └── SyntaxConfigOps.scala # Syntax highlighting operations
└── util.scala             # Utility functions
```

## Getting Started

### Prerequisites
- **Java 8+** (for sbt and Scala compilation)
- **LLVM toolchain** (for Scala Native linking)
- **sbt 1.9+** (Scala build tool)

### Building and Running

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/miueon/kilo-scala.git
    cd kilo-scala
    ```

2.  **Build the native executable:**
    ```bash
    sbt nativeLink
    # Or copy to bin/ directory:
    sbt copyToBin
    ```
    This compiles to a native executable with optimizations enabled.

3.  **Run the editor:**
    ```bash
    ./target/scala-3.7.1/kilo-scala [filename]
    # Or if using copyToBin:
    ./bin/kilo-scala [filename]
    ```

4.  **Run tests:**
    ```bash
    sbt test
    ```

### Keyboard Shortcuts
- **Ctrl+S**: Save file
- **Ctrl+Q**: Quit (press multiple times if file is modified)
- **Ctrl+F**: Find text
- **Ctrl+G**: Go to line
- **Arrow keys**: Navigate
- **Page Up/Down**: Page navigation

## Contributing

Contributions are welcome! If you find a bug or have a feature request, please open an issue on the [GitHub repository](https://github.com/miueon/kilo-scala/issues).

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
