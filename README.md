# Language Experimentation

A language that supports

- Sum types.
- Static typing.

## Immutable values

All values are immutable. However to improve ergonomics any mutating operations will 
automatically perform a copy. In reality the implementation should avoid copies 
wherever possible while maintaining the semantics of immutability.

Each parameter has two potential taints:

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

## Classes

```
pub class Vector() implements Stringer {
    pub let x, y, z float32

    // A default constructor is always provided for all public fields.
    // In this case it would be equivalent to:
    //
    //     constructor(x, y, z float32)

    pub fn length() float32 { // Pure.
        return Math.sqrt(x * x + y * y + z * z)
    }

    pub fn add(other Vector) { // Impure.
        x += other.x
        y += other.y
        z += other.z
    }

    // "override" must be specified when implementing traits.
    override pub fn string() string {
        return "Vector(#{x}, #{y}, #{z})"
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

Creates an alias for an existing type, with its own set of methods etc.


```
alias Number float {
    // TODO: Constraints?
    constraint self >= 1 && self <= 10

    fn midpoint() float {
        return this / 2.0
    }
}

```

## Channels

Channels should be used to pass values between threads.

```
let a = new chan<Vector32>()

let v = Vector32{1, 2, 3} 
v.x = 2   // Mutate
a <- v    // Copy
```

## Structural typing?

```
interface Stringer {
    fn string() string
}
```

## Actors?

```
actor Owner {
    fn feed(pet Pet) {
    }
}

// Start Actor.
let owner = Owner()
owner.feed(Pet("Bob))
```

## Traits

Static traits must be declared as implemented:

```
trait Pet {
    // Method with a default implementation. Can still be overridden.
    fn description() string {
        return "no description"
    }

    // No implementation, must be overridden.
    fn name()

    // Fields without a default value must be provided by implementations.
    let age int
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

match (result) {
value(value):
error(error):
}
```

## Pattern matching

```
let tuples = [("a", 123), ("b", 234)]

for tuple in tuples {
    match tuple {
    case ("a", n):
        println("a #{n}")

    default:
        println(tuple[0], tuple[1])
    }
}
```

```go
type StringIntTuple struct {
    A string
    B int
}
var tuples = []StringIntTuple{
    {A: "a", B: 123},
    {A: "b", B: 234},
}

for _, tuple := range tuples {
    if tuple.A == "a" {
        n := tuple.B
        fmt.Sprintf("a %d", n)
    } else {
        fmt.Println(tuple.A, tuple.B)
    }
}
```

## For loop

```
for value in array {
}

for (index, value) in array {
}

for key in map {
}

for (key, value) in map {
}

for value in set {
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
