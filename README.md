# Language Experiment

This is a playground for PL ideas. My goal is to have a safe elegant language
while maintaining simplicity. This is clearly subjective.

- Sum types.
- Static typing.
- Concurrent safety.
- ...

## First-class support for deploying

What does this mean?

- Builtin support for bundling resources, hot reload during development.
- Automatic Docker builds?

## Concurrency options

The appeal of Go's concurrency model is that any normal synchronous function
can be called asynchronously. The downside of its model is that there are no
guarantees around concurrent access - each type must manage its own synchronisation
manually. How do we solve this?

### Automatic ownership management

Like Rust, but without the boilerplate. Similar to [D](https://dlang.org/blog/2019/07/15/ownership-and-borrowing-in-d/).

Rules:

1. Any value passed to an asynchronous construct will have its ownership transferred.
2. The `copy` operator creates a clone of a value.

eg.

```
interface Component {
    fn kind(): int
}

class ECS {
    let components = new {int: [Component|None]}

    pub fn assign<C>(id: int, component: C) {
        let kind = component.kind()
        for components[kind].len() <= id {
            components[kind].append(None)
        }
        components[kind][id] = component
    }

    pub fn each<C...>(iter fn(id:int))) {
    }
}
```

### [Active Object](https://en.wikipedia.org/wiki/Active_object)

Concept: concurrent-safe facades for synchronous objects are automatically
generated by the compiler.

Both types and values can be marked as `async`.

```
async class Processor { }

let a = new Processor()

class Vector {}

let a = new Vector()
let b = async a
```

If public fields are allowed this basically necessitates getters/setters,
at least at the compiler level. Perhaps public fields should be forbidden for
asnyc values?

Non-async values are equivalent to `unique_ptr<T>` in C++.
`async` values may have multiple concurrent references. There is
no equivalent in C++.

#### Example

```
async class Redis {
    let count = 0
    let values: {string: Value}

    pub fn set(key: string, value: Value) {
        count++
        values[key] = value
    }

    pub fn get(key: string): Value? {
        return values[key]
    }
}
```

Might result in the following:

```
async class Redis {
    let count = 0
    let values: {string: Value}
    let _count_lock: RWLock
    let _values_lock: RWLock

    pub fn set(key: string, value: Value) {
        with _count_lock.acquire_rw(), _values_lock.acquire_rw() {
            count++
            values[key] = value
        }
    }

    pub fn get(key: string): Value? {
        with _values_lock.acquire_ro() {
            return values[key]
        }
    }
}
```

### Actors?

- Actor methods cannot return values.
- Invoking any method (including the constructor) on an actor enqueues the call on its mailbox.
- All messages to the actor are applied synchronously.
- Actors cannot contain public fields.
- Any value passed into an actor will transfer ownership to the actor.
- If an actor named "main" exists it will be the main entry point rather than the "main" function.

#### Actor functions?

```
actor fn poll(msg: string) {
   print(msg)
}

let p = start poll
p("hello")
```

Bit ugly?

#### Normal actors

```
actor Owner {
    let pet: Pet?

    init(pet: Pet?) {
        self.adopt(pet)
    }

    fn adopt(pet: Pet) {
        self.pet = pet
        feed("seed")
    }

    fn feed(food: string) {
        if let pet = pet {
            pet.feed(food)
        }
    }
}

actor main {
    init() {
        let pet = Pet("Lucius")
        //let pets = [pet]
        // Start Actor.
        //let owner = Owner(pets[0])   // Error: can't transfer ownership of an element.
        let owner = start Owner(pet)   // Create and start the actor.
        //pet.feed("moo")              // Error: pet is owned by "owner"

        // Send some messages.
        owner.adopt(pet)
        owner.feed("seed")

        kill(owner)
    }
}
```

[Akka example](https://alvinalexander.com/scala/how-to-communicate-send-messages-scala-akka-actors):

```scala
import akka.actor._

case object PingMessage
case object PongMessage
case object StartMessage
case object StopMessage

class Ping(pong: ActorRef) extends Actor {
    var count = 0
    def incrementAndPrint { count += 1; println("ping") }
    def receive = {
        case StartMessage =>
            incrementAndPrint
            pong ! PingMessage
        case PongMessage =>
            incrementAndPrint
            if (count > 99) {
                sender ! StopMessage
                println("ping stopped")
                context.stop(self)
            } else {
                sender ! PingMessage
            }
        case _ => println("Ping got something unexpected.")
    }
}

class Pong extends Actor {
    def receive = {
      case PingMessage =>
          println(" pong")
          sender ! PongMessage
      case StopMessage =>
          println("pong stopped")
          context.stop(self)
      case _ => println("Pong got something unexpected.")
    }
}

object PingPongTest extends App {
    val system = ActorSystem("PingPongSystem")
    val pong = system.actorOf(Props[Pong], name = "pong")
    val ping = system.actorOf(Props(new Ping(pong)), name = "ping")
    // start the action
    ping ! StartMessage
    // commented-out so you can see all the output
    //system.shutdown
}
```

In langx:

```
// Actor interfaces may only be applied to actors.
actor interface Pinger {
  fn ping(ping: Pinger)
}

actor Ping: Pinger {
    let count = 0

    pub fn start(pong: Pong) {
        incrementAndPrint()
        pong.ping(self)
    }

    pub fn ping(ping: Pinger) {
        incrementAndPrint()
        if count > 99 {
            println("ping stop")
            ping.stop()
            kill(self)
        } else {
            ping.ping(self)
        }
    }

    fn incrementAndPrint() {
        count++
        println("ping")
    }
}

actor Pong: Pinger {
    pub fn ping(ping: Pinger) {
        println("pong")
        ping.ping(self)
    }

    pub fn stop() {
        println("pong stop")
        kill(self)
    }
}

fn main() {
    let ping = Ping()
    let pong = Pong()
    ping.start(pong)
}

```

## Classes

```
pub class Vector: Stringer {
    // All fields are given default values. One difference from Go is that
    // arrays, maps, and classes are given default-constructor values.
    let x, y, z : float32

    // A default constructor is always provided for all public fields.
    // In this case it would be equivalent to:
    //
    //     constructor(x:float = 0, y:float = 0, z:float = 0)

    pub fn length():float { // Pure.
        return Math.sqrt(x * x + y * y + z * z)
    }

    pub fn add(other:Vector) { // Impure.
        x += other.x
        y += other.y
        z += other.z
    }

    // "override" can be specified when implementing traits to ensure that changes to the
    // interface don't result in methods beign orphaned.
    override pub fn string(): string {
        return "Vector({x}, {y}, {z})"
    }
}
```

## Generics

```
class Stack<T>: Iterable<T> {
    let stack: [T]   // Backed by an array.

    pub fn push(v: T) {
        stack.append(v)
    }

    // Implements Iterable<T>
    override pub fn iterator(): Iterator<T> {
        return stack.iterator()
    }
}
```

Generic functions:

```
fn map<T, U>(l: [T], f fn(v T):U): [U] {
    let out: [U]
    for v in l {
        out.append(f(v))
    }
    return out
}

let ints = [1, 2, 3]
let floats = map(ints, fn(v int) float {
    return float(v)
})
```

## Arrays

```
let a: [string]         // Explicitly typed.
let a = ["hello"]       // Type inference.
```

## Maps

```
let a: {string: Vector}              // Explicitly typed.
let b = {"hello": Vector(x:1, y:2, z:3)}   // Type inference.
```

## Sets

```
let a: {string}         // Explicitly typed.
let a = {"hello"}        // Type inference.
```

## Range

Syntactic sugar for a range type? Used for slices, for loops, etc.

```
// Equivalent.
let a = 1..3
let a = new Range<int>(1, 3)

let b = [1, 2, 3, 4, 5, 6]

// Equivalent.
let c = b[1..3] 
let c = b[a]

for n in 1..10 {
}
```

## Type aliases?

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

## Channels?

Channels should be used to pass values between threads.

```
let a = new chan<Vector32>()

let v = Vector32{1, 2, 3}
v.x = 2   // Mutate
a <- v    // Copy
```

## Interfaces


Structural typing and traditional interfaces are complementary in that the former is
more consumer-centric, while the latter is more provider-centric. To that end, langx
interfaces support both.

Interfaces are fairly straightforward:

```
interface Pet {
    // Method with a default implementation. Can still be overridden.
    fn description(): string {
        return "{name} is {age} years old"
    }

    // Methods without implementations must be provided.
    fn mood(): string

    // Fields without a default value must be provided by implementations.
    let name: string
    let age: int
}
```


Structural typing usage:

```
class Dog {
    pub fn mood(): string {
        return "happy
    }

    pub let name: string
    pub let age: int
}

let dog: Pet = Dog(name: "Fido", age: 8)
```

```
class Dog: Pet {
    pub fn mood(): string {
        return "happy
    }

    pub let name: string
    pub let age: int
}
```

## Constructors

Constructor arguments are *always* named? Positional arguments are not supported?

```
class Vector {
    let x, y, z: float

    // Static factory method.
    static fn unit(): Vector {
        return Vector(y: 1)
    }
}

fn f() {
    // A default constructor is generated if not otherwise provided.
    let a = Vector(x: 1)
}
```

## Sum types / enums

Very similar to Swift.

```
enum Result<T> {
    case Value(T)
    case Error(error)
}

enum Optional<T> {
    case None
    case Value<T>
}

let result = Result.Value("hello world")

switch (result) {
case .Value(value):
case .Error(err):
}
```

The default value for an enum is the first case, only if it is untyped. If all cases
are typed (eg. `Result<T>` above) then there is no possible default value.

Support for anonymously combining types into enums:

```
enum Option<T> {
    case Value(T)
    case None
}

// This will merge the Option<T> with error to create a single enum:
//
// enum Anonymous {
//   case Value(string)
//   case None
//   case error
// }
//
// Do we want this vs. the Option becoming a first class case?
//
// Upside is you can return any literal that can be inferred, downside is you can't
// convert to an Option.
fn f(): Option<string>|error {
    return "hello"
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

for value in channel {
}
```

## Error handling?

```
enum MyError:error {
    case IOError(io.Error)
    case UserError(string)
}

fn sub():string throws {
    return ""
}

fn function():string throws {
    if false {
        throw MyError.UserError("something is false")
    }
    let a = try sub() // Rethrow
    if let a = try sub() {
    }
    return "hello"
}
```

## Native optional type

```
// Builtin type definition.
enum Optional<T> {
    case None
    case Some<T>
}

let a = Optional.Some("hello world")

fn f() {
    // "if let" is shorthand for:
    //
    //      switch a {
    //      case .Some(b):
    //      default:
    //      }
    if let b = a {
    } else {
    }

    a = none
    // Shorthand for: a = Optional<string>.None
    a = "goodbye"
    // Shorthand for: a = Optional<string>.Some("goodbye")
}
```

## Imports

Like Go? Automatic imports?

Python style?

```
import <package>[ as <alias>][, ...]
from <package> import <symbol>[as <alias>][, ...]
```

eg.

```
import "github.com/alecthomas/participle" as parser
from "github.com/alecthomas/participle" import Parser
```

Or Go style only?

```
import <package>[ as <alias>][, ...]
```

eg.

```
import "github.com/alecthomas/participle" as parser
```

## Annotations?

Accessible via reflection. Mmmmmmmmmmm. Avoid for now, though there needs to be
some solution for eg. JSON encoding. Writing manual encoders/decoders sucks.

```
import "types"
from "types" import annotation

// Lets the compiler know that this class is intended to be an annotation.
@annotation(restrict=[types.Class, types.Field, types.Method])
class json {
    let omit:bool = false
}

class User {
    let name:string

    let email:string

    let age:int

    @json(omit=true)
    let ssn:string
}
```

## Compile time reflection

Ala [Zig](https://ziglang.org/#Compile-time-reflection-and-compile-time-code-execution). This is great.
I haven't given this much though, so I'm not sure what it would entail. An interpreter maybe?

This would actually be fairly straightforward to implement with WASM as the backend. The
compile-time code outputs WASM which is then interpreted by the compiler to generate
code/AST that is compiled again.

## Interoperability with Go/C?

If the language is hosted by the Go runtime, should it support interoperability with Go? Or C?

Pros:
- large set of existing libraries

Cons:
- how does immutability interoperate with Go?
- limits the language to constructs supported by the Go runtime


## Resource lifetimes

For objects implementing `io.Closer`, `with` will close them at the end of the block.

```
with try f = os.open("/etc/passwd") {
}
```

As with `if try`, `with` blocks can have `catch` and `rethrow` alternates.

## Error handling

Errors are reported via enums:

```
fn open(path: string): File|error {
    return error("{path} not found")
}
```

How do we handle these elegantly?

### Rust-style re-throw operator `?`?

```
let f = os.Open("/etc/passwd")?
```

Very convenient, but too magical?

### Error-specific `try` syntax in `if` and `with` blocks?

```
if|with try [<var> = ] <expr> <block>
catch [[<var>:]<type>] <block>
rethrow
```

```
with try file = os.open("/etc/passwd") {
    let scanner = new bufio.Scanner(file)
    for scanner.scan() {
        println(scanner.text())
    }
    return scanner.err()
} catch os.ErrNotExist {
    return
} rethrow

with try file = os.open("/etc/passwd") {
}

if try os.stat("/etc/passwd") {
} catch {
}
```

```rust
use std::io;
use std::fs;

fn read_username_from_file() -> Result<String, io::Error> {
    fs::read_to_string("hello.txt")
}
```

```
import ioutil

fn readUsernameFromFile(): bytes|error {
    return ioutil.readFile("hello.txt")
}

// <type>? is a shortcut for <type>|none
fn username(): string? {
    return "bob"
}
```


## Examples

### Templated Enum

```
enum Option<T> {
    case Some(T)
    case None
}

let a : Option<string> = "hello"
let b : Option<String> = none
```


### Entity Component System

```
class Vector {
    let x, y, z : float
}

class Base {
    let position, direction : Vector
    let opacity : float
}

class Script {
    let source : string
}

enum Component {
    case Base(Base)
    case Script(Script)

    let slot() : int {
        switch self {
        case Base(_): return 0
        case Script(_): return 1
        }
    }
}

class ECS {
    let free : [int]
    let entities : [[Component?]]

    // Create a new Entity.
    fn create() : int {
        if (free.size() > 0) {
            return free.pop()
        }
        let id = entities.size()
        entities.append([])
        return id
    }

    fn delete(id : int) {
        free.push(id)
        entities[id] = []
    }

    fn assign(id : int, component : Component) {
        let components = entities[id]
        let slot = component.slot()

        if (slot >= components.size()) {
            components.resize(slot + 1)
        }
        components[slot] = component
    }

    fn unassign(id : int, component : Component) {
        entities[id][component.slot()] = none
    }
}
```

## Redis

```go
type Scalar interface { scalar() }
type Float float64
func (Float) scalar() {}
type String string
func (String) scalar() {}
type Bool bool
func (Bool) scalar() {}

type Value struct {
    Scalar Scalar
    List []Scalar
    Hash map[string]Scalar
}
```

```
type Scalar = float|string|bool

enum Value {
case Scalar(Scalar)
case List([Scalar])
case Hash({string: Scalar})
}

class Redis {
    let values: {string: Value}

    fn len(key: string): int? {
        let value = values[key] else return
        switch value {
        case .List(list):
            return list.size()

        case .Hash(hash):
            return hash.size()

        case .Scalar(scalar):
            if let str = scalar as .string {
                return str.size()
            }
        }
    }

    // Append one or more elements to a list.
    fn rpush(key: string, scalar: Scalar): error?
        if let value = values.setDefault(key, .List([])) {
            list.append(scalar)
        } else {
            return error("expected a list at {key}")
        }
    }

    // Length of a list.
    fn llen(key: string): int? {
        if let value = values[key]; list = value as .List {
            return list.size()
        }
    }

    fn lindex(key: string, index: int): Scalar? {
        if let value = values[key]; list = value as .List {
            return list[index]
        }
    }

    // Set a field of a hash value.
    fn hset(key: string, field: string, value: Scalar): error? {
        if let value = values.setDefault(key, .Hash({})) {
            hash[field] = value
        } else {
            return error("expected a hash at {key}")
        }
    }

    fn hget(key: string, field: string): Scalar? {
        if let value = values[key]; hash = value as .Hash {
            return hash[field]
        }
    }

    fn hlen(key: string): int? {
        if let value = values[key]; hash = value as .Hash {
            return hash.size()
        }
    }

    fn del(key: string) {
        values.delete(key)
    }

    fn exists(key: string): bool {
        return values.contains(key)
    }
}
```

