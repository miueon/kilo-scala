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

*   [**Scala 3**](https://docs.scala-lang.org/scala3/): A modern, multi-paradigm programming language.
*   [**Scala Native**](https://scala-native.org/): Compiles Scala code to native executables.
*   [**Cats**](https://typelevel.org/cats/): A library for functional programming in Scala.
*   [**Cats Effect**](https://typelevel.org/cats-effect/): A library for managing side effects in a functional way.
*   [**sbt**](https://www.scala-sbt.org/): The interactive build tool for Scala.

## Design and Architecture

Kilo-Scala is built with a focus on functional programming and a clean, modular architecture.

*   **Purely Functional:** The application is written in a purely functional style, using the `IO` monad from Cats Effect to handle all side effects. This makes the code more predictable, testable, and easier to reason about.
*   **State Management:** The editor's state is managed using the `StateT` monad transformer, which allows for a clean separation of state and logic.
*   **Event-driven:** The application uses an event-driven architecture to handle user input and other events asynchronously. A custom `HybridQueue` is used for efficient event processing.
*   **Foreign Function Interface (FFI):** Scala Native's FFI is used to interact with the underlying C library for raw terminal mode.
*   **Modular and Declarative:** The codebase is organized into modules for different concerns (e.g., `domain`, `effect`, `services`), and the code is written in a declarative style.
*   **Resource Safety:** The `Resource` data type from Cats Effect is used to ensure that resources like files and terminal raw mode are managed safely and reliably.

## Getting Started

To build and run the project, you need to have Scala and sbt installed.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/miueon/kilo-scala.git
    cd kilo-scala
    ```

2.  **Build the native executable:**
    ```bash
    sbt copyToBin
    ```
    This will build the native executable and copy it to the `bin/` directory.

3.  **Run the editor:**
    ```bash
    ./bin/kilo-scala [filename]
    ```
    If you don't provide a filename, it will open an empty buffer.

## Contributing

Contributions are welcome! If you find a bug or have a feature request, please open an issue on the [GitHub repository](https://github.com/miueon/kilo-scala/issues).

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
