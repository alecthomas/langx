# Language Experimentation

A language that supports

- Sum types.
- Static typing.

## Immutable values

All values are immutable. However to improve ergonomics any mutating operations will 
automatically perform a copy. In reality the implementation should avoid copies 
wherever possible while maintaining the semantics of immutability.

Each parameter has two potential tains:

1. A mutation taint that states whether the function mutates the 
   parameter value.
2. An asynchronous taint that states whether the parameter is passed to an
   asynchronous construct.
   
Taints are transitive.

```
// This function does not mutate "s", does not pass "s" to any functions that do,
// and does not create any threads referencing "s". This guarantees that dump() is
// pure and synchronous.
fn dump(s Stack<string>) {
    for v in s {
        println(v)
    }
}

let a = new Stack<string>()
a.push("a")     // Mutate.
dump(a)         // Reference (synchronous+pure).
go dump(a)      // Copy (asynchronous).
a.x = 1.11      // Mutate.
```

## Product types (structs)

```
struct Vector {
    let x, y, z float32

    fn length() float32 { // Pure.
        return Math.sqrt(x * x + y * y + z * z)
    }

    fn add(other Vector) { // Impure.
        x += other.x
        y += other.y
        z += other.z
    }
}
```

When methods mutate the object state, the method's implicit "self" is give a mutable taint.

## Generics

```
struct Stack<T> {
    let stack [T]   // Backed by an array.

    public fn push(v T) {
        stack.append(v)
    }

    // Implements Iterable<T>
    public fn iterator() Iterator<T> {
        return stack.iterator()
    }
}
```

## Arrays

```
let a = new [string]    // Explicitly typed.
let a = ["hello"]       // Type inference.
```

## Maps

```
let a = new {string: Vector}         // Explicitly typed.
let b = {"hello": Vector{1, 2, 3}}   // Type inferencu.
```

## Sets

```
let a = new {string}     // Explicitly typed.
let a = {"hello"}        // Type inference.
```

## Type aliases

```
struct List<T> {}

alias StringList List<string>
```

## Channels

Channels should be used to pass values between threads.

```
let a = new chan<Vector32>()

let v = Vector32{1, 2, 3} 
v.x = 2   // Mutate
a <- v    // Copy
```

## Interfaces

Structural typing ala Go.

```
interface Stringer {
    string(): string
}
```

## Sum types

Very similar to Swift.

```
enum Result<T> {
    case value(T)
    case error(error)
}

enum Optional<T> {
    case value<T>
    case none
}

let result = Result.value("hello world")

match result {
value(value):
error(error):
}
```

## Error handling

```
fn function() throws string {
    if false {
        throw AnError
    }
    return "hello"
}
```
