# Nail

## Summary

__WIP__

🚀 Introducing Nail!  
Merging the robustness of Rust with the simplicity of Go's runtime.  
Just like Rust, if it compiles, it's likely to run bug-free (of course, not all bugs can be eliminated).
A memory-safe language powered by GC, embracing Rust's type system (with a touch of TypeScript) and concurrent model inspired by Go.  
Step into the future with a language inspired by the best of Rust and Go. A refreshing take on modern programming!

- Base: Rust
- Memory Management: GC + optimization
- Type System: Rust, with hints of TypeScript
- Concurrency: Go

🔧 Note: While our vision remains consistent, the specific implementations to achieve our goals may undergo significant changes.

## Memory Management Strategy

__Mutable Data__

- __Large Size__: Pass by pointer  
  For large data structures, passing by pointer avoids the cost of copying the entire data.
- __Small Size__: Pass by pointer  
	Even for small data, passing by pointer ensures consistent data management for mutable data.

__Immutable Data__

- __Large Size__: Pass by pointer  
	For large immutable data, passing by pointer provides efficient access without the performance overhead of copying.
- __Small Size__: Pass by value  
	Small immutable data is copied to the stack, resulting in faster memory access.

This approach ensures efficient memory management and optimized performance by automatically selecting the most appropriate method based on data size and mutability.

## Specification

Implemented = ✅, Partially Implemented = 🚧, Not Implemented = ❌

1.  Variables and Data Types

- ✅ Variable definition and referencing
  ```rust
  let value = 10;
  value // 10
  ```
- Basic data types, such as integers, floating-point numbers, booleans, and characters
  - 🚧 Integer types  
    Integer literals can be written as 10. To specify the number of bits, add the type name to the end, like 10_i8. If the type cannot be determined through type inference, it defaults to i32.
    | Length   | Signed | Unsigned |
    | -------- | ------ | -------- |
    | 8-bits   | i8     | u8       |
    | 16-bits  | i16    | u16      |
    | 32-bits  | i32    | u32      |
    | 64-bits  | i64    | u64      |
    | 128-bits | i128   | u128     |
    | arch     | isize  | usize    |
  - ❌ Floating-point types  
    Floating-point literals can be written as 10.0. To specify the number of bits, add the type name to the end, like 10_f32 or 10.0_f32. If the type cannot be determined through type inference, it defaults to f64.
    | Length   | -    |
    | -------- | ---- |
    | 32-bits  | f32  |
    | 64-bits  | f64  |
    | 128-bits | f128 |
  - ✅ Boolean types  
    Boolean literals can be written as true and false.
  - 🚧 Character types  
    Character literals can be written as 'a'.
- Collection types, such as strings, arrays, lists, tuples, and dictionaries
  - ✅ String types  
    Strings are allocated on the heap.
    String literals can be written as "Hello, world!".
  - ❌ Array types  
    Arrays are allocated on the stack.
    Array literals can be written as [1, 2, 3].
  - ❌ List types  
    Lists are dynamic arrays and are allocated on the heap.
    List literals can be written as vec![1, 2, 3].
  - ❌ Tuple types  
    Tuple literals can be written as (1, "hello", 3.14).
  - ❌ Dictionary types  
    Dictionary literals can be written as map!{ "a": 1, "b": 2, "c": 3 }.
- Defining and using custom types, such as structs, enums, and algebraic data types
  - ❌ Struct types  
    ```rust
    struct User {
        name: String
    }
    ```
  - ❌ Enum types
    ```rust
    enum ShapeType {
        Circle,
        Rectangle,
    }
    ```
  - ❌ Algebraic data types
    ```rust
    enum Shape {
        Circle(f32),
        Rectangle(f32, f32),
        Triangle { base: f32, height: f32 }
    }
    ```

2. Operators

- ✅ Arithmetic operators (addition, subtraction, multiplication, division, etc.)
  ```rust
  1 + 2; // add
  1 - 2; // sub
  1 * 2; // mul
  1 / 2; // div
  ```
- ✅ Comparison operators (equality, inequality, greater/less than, etc.)
  ```rust
  1 == 2; // equal
  1 != 2; // not equal
  ```
- ❌ Logical operators (AND, OR, NOT, etc.)
  ```rust
  true && true // and
  false || true // or
  !true // not
  ```
- 🚧 Assignment operators
  Only mutable variables can be assigned.
  ```rust
  let mut a = 10; // define variable
  a = 20;
  ```
- ❌ Bitwise operators (bitwise AND, OR, NOT, shift, etc.)
  ```rust
  1 & 0 // bitwise and
  1 | 0 // bitwise or
  1 << 2 // bit left shift
  2 >> 1 // bit right shift
  ```

3. Control structures

- Conditional branching (if, else, match, etc.)

  - ✅ if
    ```rust
    if true {
        // statements
    }
    ```
  - ✅ else
    ```rust
    if false {
        // statements
    } else {
        // statements
    }
    ```
  - ❌ match

    ```rust
    enum Shape {
        Circle(f32),
        Rectangle(f32, f32),
        Triangle { base: f32, height: f32 }
    }

    let shape = ShapeType::Circle;
    match shape {
        Shape::Circle(radius) => (), // expression
        Shape::Rectanble(width, height) => {
            // statements
        }
        Triangle { base: f32, height: f32 } => {
            // statements
        }
    }
    ```

  - Loops (for, while, etc.)
    - ❌ for
      ```rust
      for value in [1, 2, 3] {
          // statements
      }
      ```
    - ❌ while
      ```rust
      let value = 1;
      while value < 10 {
          // statements
      }
      ```
    - ✅ loop
      ```rust
      loop {
          // statements
      }
      ```
    - Jumps (break, continue, return, etc.)
      - ✅ break  
        Exits the current loop context.
        ```rust
        loop {
            break;
        }
        ```
      - ✅ continue  
        Preserves the current loop context and skips the processing after continue, proceeding to the next iteration.
        ```rust
        loop {
            continue;
            // no execution
        }
        ```

- ✅ return  
  Interrupts the current function and returns a value. If no value is specified, () is returned.
  ```rust
  fn return_unit() {
    return;
  }
  fn return_value() -> i32 {
    return 10;
  }
  ```

2.  Functions and Methods

- ✅ Function definition and invocation

  ```rust
  fn function() -> i32 {
      return 10
  }

  function();
  ```

- ❌ Struct function definition and invocation  
  You can define functions in the struct scope using impl.  
  Defined functions can be called by using struct_name::function_name.  
  By specifying self as the first argument, object-oriented style method invocation is possible.

  ```rust
  struct Rect {
    x: i32,
    y: i32
  }
  impl Rect {
    fn new(x: i32, y: i32) -> Rect {
      Rect {
        x: x,
        y: y
      }
    }

    fn area(self) -> i32 {
        self.x * self.y
    }
  }
  let rect = Rect::new(2, 3);
  rect.area(); // 6
  ```

- ❌ Lambda expressions

  ```rust
  let value = || 10;
  value(); // 10

  let sum = |x, y| x * y;
  sum(2, 3); // 6

  let block = ||　{
      let x = 10;
      let y = 20;
      x + y
  };
  block(); // 30

  let capture_value = 10;
  let closure = || {
      capture_value + 20
  };
  closure(); // 30
  ```

3. Modules and Packages

- 🚧 File modules

  ```rust
  //- /file_mod.nail
  fn function_in_file_mod() -> i32 {
      10
  }

  //- /main.nail
  mod file_mod;
  ```

- 🚧 In-file modules
  ```rust
  mod module_in_file {
      fn function() -> i32 {
          10
      }
  }
  ```
- 🚧 Module reference  
  Reference by specifying the path directly or using use. The reference method is the same for both file modules and in-file modules.

  ```rust
  mod module {
    fn function() -> i32 {
      10
    }
  }
  fn main() {
    module::function(); // 10

    use module::function;
    function(); // 10
  }
  ```

- ❌ Package management and dependency resolution  
  Undefined

4. Error Handling

- ❌ Recoverable Errors  
  Recoverable errors are just one of the values. The Result type is used to indicate success or failure. The Result type allows for early returns of errors using ? and extraction of the value on success.
  ```rust
  enum Error {
      SomeError
  }
  fn operate() -> Result<String, Error> {
      Result::Err(Error::SomeError)
  }
  fn main() -> Result<String, Error> {
      let result = match operate {
          Ok(value) => value,
          Err(err) => return err,
      };
      // or
      let result = operate()?;
  }
  ```
- ❌ Unrecoverable Errors  
  Unrecoverable errors cause the call stack to unwind, and the application terminates. There are plans to provide ways to catch unrecoverable errors, but they are intended for library developers and not for use in application code.
  ```rust
  panic!("Some error.");
  ```

5. Type System

- 🚧 Static Typing and Dynamic Typing  
  Undefined
- 🚧 Type Inference  
  Based on the Hindley–Milner type system, it features bidirectional type inference.
  ```rust
  fn value(x: i64) -> i64 {
      x
  }
  let a = 10;
  x(a) // type of a is i64
  ```
- ❌ Generics  
  Generics can be used with function definitions, structs, algebraic data types, and in impl blocks.
  ```rust
  fn swap<T>(x: T, y: T) -> (T, T) {
      (y, x)
  }
  struct Vec<T> {
      data: [T],
  }
  enum Result<T, E> {
      Ok(T),
      Err(E),
  }
  impl<T, E> Result<T, E> {
      fn unwrap(self) -> T {
          match self {
              Result::Ok(value) => value,
              Result::Err(err) => panic!(err),
          }
      }
  }
  ```

6. Memory Management

   - ❌ Garbage Collection
     Undefined
   - ❌ Manual Memory Management
     Undefined

7. Concurrency and Parallelism

   - ❌ Creating and running threads or tasks  
     Undefined

   - ❌ Synchronization primitives (mutexes, semaphores, condition variables, etc.)  
     Undefined

   - ❌ Asynchronous programming  
     Undefined

   - ❌ Concurrency models  
     Nail supports the CSP concurrency model and has an asynchronous runtime. By default, functions are executed in the asynchronous runtime, so there is no need to mark individual asynchronous functions. Use spawn for non-blocking execution; otherwise, it will be a blocking execution.
     The work-stealing algorithm is continuation stealing.
     ```rust
     fn async_function() -> i32 {
       10
     }
     fn main() {
       async_function(); // await
       async_function().async;
     }
     ```

8. Input/Output

- ❌ Access to standard input, standard output, and standard error output  
  Undefined
- ❌ Reading and writing files  
  Undefined
- ❌ Network communication (TCP/IP, UDP, HTTP, etc.)  
  Undefined

9. String manipulation

- ❌ String concatenation, splitting, replacement, searching, etc.  
  Undefined
- ❌ Regular expressions  
  Undefined

10. Traits

- ❌ Definition  
  Undefined
- ❌ Invocation  
  Undefined

11. Meta-programming (as needed)

- ❌ Macros  
  Undefined
- ❌ Compile-time computation  
  Undefined

12. Debugging and profiling

- ❌ Debugging features (breakpoints, step execution, variable monitoring, etc.)  
  Undefined
- ❌ Profiling features (performance measurement, memory usage analysis, etc.)  
  Undefined

13. Standard library

- ❌ A library providing general features such as mathematical functions, date and time manipulation, collection operations, etc.  
  Undefined

14. Parallel compilation  
    Per-file or per-pod(package) basis

## Experimental

It is a specification that I am currently considering whether to include it as a language feature.

- Partial Type  
Support structural subtyping.
  ```
  partial type V4IpAddress {
      address: Vec<u8>
  }

  partial type V6IpAddress {
      address: Vec<u8>
  }

  fn is_empty(ip_address: V4IpAddress) -> bool {
      ip_address.address.is_empty()
  }

  fn handle(ip_address: V4IpAddress) -> bool {
      match ip_address {
          // IpAddressV4 and IpAddressV6 cannot be matched because I don't know what to pass to IpAddress.
          // It is close to the concept of Trait and Interface.
          // It might be called an implicitly implemented Trait or Interface.
      }
  }

  let ip_address = V4IpAddress {
      address: vec![127, 0, 0, 1]
  };
  is_empty(ip_address); // false

  let ip_address = V6IpAddress {
      address: vec![0, 0, 0, 0, 0, 0, 0, 1]
  };
  is_empty(ip_address); // false
  ```

- Nested type definition  
Support for type definitions with nested structures such as Json
  ```
  type Json {
      key1: {
          key2: {
              key3: String
          }
      },
      key4: String
  }

  let json = Json {
      key1: .{ // This has the problem of being indistinguishable from BlockExpr. So let's add.
          key2: .{
              key3: "value"
          }
      },
      key4: "value"
  };
  json.key1.key2.key3; // "value"
  json.key4; // "value"
  fn handle(json: Json) -> bool {
      json.key1.key2.key3; // "value"
  }
  ```

- Reference / Move / Copy semantics

  ```rust
  struct NotCopyable(i32);

  struct Copyable(i32);
  impl Copy for Copyable {}

  struct Custom {
    not_copyable: NotCopyable
    copyable: Copyable
    auto_copyable: i32
  }
  impl Custom {
    fn new() -> *Custom {
      *Custom { a: 10 }
    }

    /** Custom */
    fn ref_self(self) -> Custom {
      self
    }

    fn move_self(*self) -> *Custom {
      *self
    }

    fn copy_self(*self) -> *Custom {
      *self.copy() // Error
    }

    /** NotCopyable */
    fn ref_not_copyable(self) -> NotCopyable {
      self.not_copyable
    }

    fn move_not_copyable(*self) -> *NotCopyable {
      *self.not_copyable
    }

    fn copy_not_copyable(self) -> *NotCopyable {
      *self.not_copyable.copy() // Error
    }

    /** Copyable */
    fn ref_copyable(self) -> Copyable {
      self.copyable // Error
    }

    fn move_copyable(*self) -> *Copyable {
      *self.copyable // Error
    }

    fn copy_copyable(self) -> *Copyable {
      self.copyable.copy() // or *self.b
    }

    /** AutoCopyable */
    fn ref_auto_copyable(self) -> i32 {
      self.auto_copyable // Auto copy
    }

    fn move_auto_copyable(self) -> i32 {
      *self.auto_copyable // Error
    }

    fn copy_auto_copyable(self) -> i32 {
      self.auto_copyable // or self.auto_copyable.copy()
    }
  }

  // Cannot be referenced.
  // Must always call .copy().
  trait Copy: Clone {
  }

  // This is an automatic copy.
  // No need to call .copy().
  // Essentially, only primitive types implement this.
  // note: On the IDE, want to change the color
  trait AutoCopy: Copy {
  }
  ```

- Reference / Copy semantics  
Consider that it is always a copy of the reference.

  ```rust
  struct NotCopyable(i32);

  struct Copyable(i32);
  impl Copy for Copyable {}

  struct Custom {
    not_copyable: NotCopyable
    copyable: Copyable
    auto_copyable: i32
  }
  impl Custom {
    fn new() -> *Custom {
      *Custom { a: 10 }
    }

    /** Custom */
    fn ref_self(self) -> Custom {
      self
    }

    fn copy_self(self) -> Custom {
      self.copy() // Error
    }

    /** NotCopyable */
    fn ref_not_copyable(self) -> NotCopyable {
      self.not_copyable
    }

    fn copy_not_copyable(self) -> NotCopyable {
      self.not_copyable.copy() // Error
    }

    /** Copyable */
    fn ref_copyable(self) -> Copyable {
      self.copyable
    }

    fn copy_copyable(self) -> Copyable {
      self.copyable.copy() // or *self.copyable
    }

    /** AutoCopyable */
    fn ref_auto_copyable(self) -> i32 {
      self.auto_copyable // Auto copy, no reference
    }

    fn copy_auto_copyable(self) -> i32 {
      self.auto_copyable // or self.auto_copyable.copy() or *self.auto_copyable
    }
  }
  ```

  It can also consume itself or its arguments.

  ```rust
  impl Custom {
    fn consume_self(consume self) {
      // After that, self cannot be accessed.
    }

    fn consume_arg(self, x: consume i32) {
      // The x passed to this function will be consumed.
    }
  }
  ```

## Contributors

- [ktanaka101](https://github.com/ktanaka101) - creator, maintainer

## License

MIT
