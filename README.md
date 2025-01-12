# Nail

## Summary

**WIP**

Nail is a next-generation language inspired by Rust, but with a built-in garbage collector.  
Like Rust, we aim for “if it compiles, it will likely run reliably,” while recognizing that no language can guarantee complete freedom from bugs.  
By merging Rust’s robust type system with a GC-based memory model—and adopting certain TypeScript-like features for front-end development—Nail strives to be a practical choice for everything from server-side projects to web front ends.  
We also provide an `async/await` concurrency model to simplify asynchronous programming across various use cases (excluding embedded targets).

- **Base**: Rust
- **Memory Management**: GC + possible ownership-based optimizations
- **Type System**: Rust-inspired, with TypeScript-like additions
- **Concurrency**: async/await

> **Note**: While our core vision remains steady, the specific implementation details may evolve significantly.

## Target Domains

Nail is an experimental, GC-enabled language inspired by Rust. While still in early development, our primary focus is on:

- **Console (CLI) Applications**  
- **Desktop Applications**  
- **Web Back-End**  
- **Web Front-End** (e.g., compiling to WebAssembly)  
- **Mobile (planned)** – taking cues from Dart/Flutter

> **Note**: We do **not** plan to support heavily resource-constrained embedded devices.

---

## Additional Concepts

- **Default GC with Ownership-Based Optimization**  
  By default, we plan to rely on a garbage collector. However, if the compiler can determine an object’s lifetime or ownership precisely, it may skip the GC step for those objects to improve performance.

- **Keyword & Default Arguments**  
  We plan to support **keyword arguments** and **default arguments** for better readability—especially useful when dealing with UI-like or large-parameter-function scenarios.

- **Async/Await Syntax**  
  We aim to provide an `async/await` style similar to C# or JavaScript. Since this language is GC-based, we will avoid exposing detailed lifetime mechanics, offering a more high-level asynchronous programming experience.

- **Trait-Based Partial Subtyping (Low Priority)**  
  We are exploring a design that automatically implements a trait if a type structurally meets the trait’s requirements. However, this is a **low-priority** feature for now and may change or be dropped.

- **No Macros (For Now)**  
  We have decided not to include macros in the early stages of development, aiming to keep the language simple and maintainable. We may revisit macros in the future.

- **Potential Dynamic Sections**  
  To handle unknown (JSON-like) data, we are considering “dynamic sections” in which static type checks are relaxed, and runtime checks ensure safety. This idea is still under consideration.

- **WebAssembly Compilation**  
  We plan to use LLVM as the primary backend for WebAssembly. We will explore optimizations and suitable GC strategies to ensure high performance on WASM.

- **AI-Friendly Language Design**  
  We would like to consider language specifications that are compatible with recent generation AI.

---

## Specification

The statuses below reflect our current progress:
**Implemented = ✅, Partially Implemented = 🚧, Not Implemented = ❌**

### 1. Variables and Data Types

- ✅ **Variable definition and referencing**  
  ```rust
  let value = 10;
  value // 10
  ```

- **Basic data types** (integers, floats, booleans, chars)  
  - 🚧 **Integer types**  
    Default is `i32`. You can specify bit width, for example `10_i8`.  
    | Length   | Signed | Unsigned |
    | -------- | ------ | -------- |
    | 8-bits   | i8     | u8       |
    | 16-bits  | i16    | u16      |
    | 32-bits  | i32    | u32      |
    | 64-bits  | i64    | u64      |
    | 128-bits | i128   | u128     |
    | arch     | isize  | usize    |
  - ❌ **Floating-point types**  
    Default is `f64`. For example, `10.0_f32`.  
    | Length   | -    |
    | -------- | ---- |
    | 32-bits  | f32  |
    | 64-bits  | f64  |
    | 128-bits | f128 |
  - ✅ **Boolean types**  
    `true` and `false`.
  - 🚧 **Character types**  
    Written as `'a'`.

- **Collection types** (strings, arrays, lists, tuples, dictionaries)  
  - ✅ **String types**  
    Allocated on the heap. Written as `"Hello, world!"`.
  - ❌ **Array types**  
    Allocated on the stack. For example, `[1, 2, 3]`.
  - ❌ **List types**  
    Dynamic arrays on the heap. For example, `vec![1, 2, 3]`.
  - ❌ **Tuple types**  
    For example, `(1, "hello", 3.14)`.
  - ❌ **Dictionary types**  
    For example, `map!{"a": 1, "b": 2}`.

- **Custom types** (structs, enums, ADTs)  
  - ❌ **Struct types**  
    ```rust
    struct User {
        name: String
    }
    ```
  - ❌ **Enum types**  
    ```rust
    enum ShapeType {
        Circle,
        Rectangle,
    }
    ```
  - ❌ **Algebraic data types**  
    ```rust
    enum Shape {
        Circle(f32),
        Rectangle(f32, f32),
        Triangle { base: f32, height: f32 }
    }
    ```

### 2. Operators

- ✅ **Arithmetic operators** (`+`, `-`, `*`, `/`, etc.)  
  ```rust
  1 + 2;
  1 - 2;
  1 * 2;
  1 / 2;
  ```
- ✅ **Comparison operators** (`==`, `!=`, `<`, `>`, etc.)  
  ```rust
  1 == 2;
  1 != 2;
  ```
- ❌ **Logical operators** (`&&`, `||`, `!`, etc.)  
- 🚧 **Assignment operators**  
  ```rust
  let mut a = 10;
  a = 20;
  ```
- ❌ **Bitwise operators** (`&`, `|`, `<<`, `>>`, etc.)

### 3. Control structures

- **Conditionals** (`if`, `else`, `match`, etc.)
  - ✅ **if / else**  
    ```rust
    if condition {
        // ...
    } else {
        // ...
    }
    ```
  - ❌ **match**  
    ```rust
    enum Shape {
        Circle(f32),
        Rectangle(f32, f32),
        Triangle { base: f32, height: f32 }
    }
    match shape {
        Shape::Circle(r) => (),
        Shape::Rectangle(w, h) => (),
        Shape::Triangle { base, height } => (),
    }
    ```

- **Loops** (`for`, `while`, `loop`)  
  - ❌ **for**  
  - ❌ **while**  
  - ✅ **loop**  
    ```rust
    loop {
        // ...
    }
    ```

  - **Jump statements** (`break`, `continue`, `return`)
    - ✅ **break**  
      ```rust
      loop { break; }
      ```
    - ✅ **continue**  
      ```rust
      loop { continue; }
      ```
    - ✅ **return**  
      ```rust
      fn return_value() -> i32 {
          return 10;
      }
      ```

### 4. Functions and Methods

- ✅ **Function definition and invocation**  
  ```rust
  fn function() -> i32 {
      10
  }
  function();
  ```
- ❌ **Struct function definition (impl)**  
- ❌ **Lambda expressions (closures)**  

### 5. Modules and Packages

- 🚧 **File modules / In-file modules**  
  ```rust
  // file_mod.nail
  fn function_in_file_mod() -> i32 { 10 }

  // main.nail
  mod file_mod;
  ```
- 🚧 **Module reference**  
  ```rust
  mod module {
    fn function() -> i32 { 10 }
  }
  fn main() {
    module::function(); // 10

    use module::function;
    function(); // 10
  }
  ```
- ❌ **Package management**  
  Not yet defined

### 6. Error Handling

- ❌ **Recoverable Errors**  
  Likely to adopt `Result<T, E>` with `?` for early returns.
- ❌ **Unrecoverable Errors**  
  Likely to adopt `panic!("...")`.

### 7. Type System

- 🚧 **Static Typing / Dynamic Typing**  
  Under consideration
- 🚧 **Type Inference**  
  Inspired by Hindley–Milner with bidirectional inference.
- ❌ **Generics**  
  Will be supported for functions, structs, enums, and impl blocks.
- ❌ **Traits**
  WIP

### 8. Memory Management

- ❌ **Garbage Collection**  
  Undecided details (with potential ownership-based optimizations).
- ❌ **Manual Memory Management**  
  Not planned yet, subject to discussion

### 9. Concurrency and Parallelism

❌
In this language, calling an asynchronous function immediately starts its process.
You have two ways to handle the result:

1. **Immediate Await (`.await`)**  
   The function call returns a value, waiting right away:
   ```pseudo
   let result = fetch_data("https://example.com").await;
   // Execution continues after fetch_data completes
   ```

2. **Async First, Await Later (`.async` → `.await`)**  
   Start the process and receive a `Future`, then wait for it when you need:
   ```pseudo
   let future = fetch_data("https://example.org").async;
   // ... do other work ...
   let result = future.await;
   ```

#### Benefits
- **Clarity**: You explicitly show whether you wait immediately or retrieve the result later.  
- **Flexibility**: You can launch multiple async tasks and await them at the right time.  
- **Error Prevention**: The compiler warns if you ignore a returned `Future` without handling or awaiting it.

Use `.await` to get the result right away, or `.async` when you need concurrent execution and plan to await later.

```rust
async fn main() {
    let result = async {
        let a = async { 1 };
        let b = fetch_data().async;
        a.await + b.await
    }.await; // 3
}
async fn fetch_data() -> i64 {
    let data = async {
        // fetch data from the network
        2
    }.await;
    return data;
}
```

---

## Contributors

- [ktanaka101](https://github.com/ktanaka101) — creator, maintainer

## License

MIT
