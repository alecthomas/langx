# Language Experiment

This is a playground for PL ideas. My goal is to have a safe elegant language 
while maintaining simplicity. This is clearly subjective.

- Sum types.
- Static typing.
- Concurrent safety.
- ...


## Concurrency options

### Immutable values?

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

### Automatic ownership management

Like Rust, but without the boilerplate.

Rules:
1. Any non-actor value passed to an actor will have its ownership transferred.
2. The `copy` operator creates a clone of a value.
3. Elements of compound types (fields, array values, etc.) cannot be transferred,
   only standalone values.

### Actors?

- Actor methods cannot return values.
- Invoking any method (including the constructor) on an actor enqueues the call on its mailbox.
- All messages to the actor are applied synchronously.
- Actors cannot contain public fields.
- Any value passed into an actor will transfer ownership to the actor.
- If an actor named "main" exists it will be the main entry point rather than the "main" function.

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
        let owner = Owner(pet)         // Create and start the actor.
        //pet.feed("moo")              // Error: pet is owned by "owner"

        // Send some messages.
        owner.adopt(pet)
        owner.feed("seed")

        kill(owner)
    }
}
```

This compiles to the following Go code.

```
func Kill(actor Actor, err error) {
    actor.Supervisor().Kill(actor, err)
}

type Supervisor interface {
    Kill(actor Actor, err error)
}

type Actor interface {
    // Get the Supervisor for the Actor.
    Supervisor() Supervisor
    // Triggered when the Actor is being killed. Should perform cleanup.
    OnKill()
}

type ActorImpl struct {
    mailbox    chan func()
    supervisor Supervisor
}

type Owner struct {
    // Actor state.
    pet *Pet
}


func NewOwner(supervisor Supervisor, pet *Pet) *Owner {
    o := &Owner{
        mailbox: make(chan func(), 100),
        supervisor: supervisor,
    }
    go o.run()
    o.send(func() { o.fnInit(pet) })
    return o
}

func (o *Owner) Supervisor() Supervisor {
    return o.supervisor
}

func (o *Owner) OnKill() {
    close(o.mailbox)
}

// Actor main loop.
func (o *Owner) run() {
    for msg := range o.mailbox {
        msg()
    }
}

func (o *Owner) send(f func()) {
    select {
    case o.mailbox <- f:
    default:
        actors.Kill(o, errors.New("mailbox full"))
    }
}

func (o *Owner) SendAdopt(pet *Pet) { o.send(func() { o.fnAdopt(pet) }) }

func (o *Owner) SendFeed(food string) { o.send(func() { o.fnFeed(food) }) }

func (o *Owner) SendStop() { o.send(func() { o.fnStop() }) }

func (o *Owner) fnInit(pet *Pet) {
    o.fnAdopt(pet)
}

func (o *Owner) fnAdopt(pet *Pet) {
    o.pet = pet
    o.fnFeed("seed")
}

func (o *Owner) fnFeed(food string) {
    if pet := o.pet; pet != nil {
        pet.Feed(food)
    }
}

type Main struct {
    ActorImpl
}

func main() {
    pet := NewPet()
    owner := NewOwner(pet)
    owner.SendAdopt(pet)
    owner.SendFeed("seed")
    actors.Kill(owner, nil)
}
```

[Akka example](https://alvinalexander.com/scala/how-to-communicate-send-messages-scala-akka-actors) translated:

```
actor Ping {
    let count = 0

    pub fn start(pong: Pong) {
        incrementAndPrint()
        pong.ping(self)
    }

    pub fn ping(pong: Pong) {
        incrementAndPrint()
        if count > 99 {
            println("ping stop")
            pong.stop()
            kill(self)
        } else {
            pong.ping(self)
        }
    }

    fn incrementAndPrint() {
        count++
        println("ping")
    }
}

actor Pong {
    pub fn ping(ping: Ping) {
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
    let x, y, z: float32

    // A default constructor is always provided for all public fields.
    // In this case it would be equivalent to:
    //
    //     constructor(x, y, z float32)

    pub fn length(): float32 { // Pure.
        return Math.sqrt(x * x + y * y + z * z)
    }

    pub fn add(other: Vector) { // Impure.
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
let b = {"hello": Vector(x:1, y:2, z:3)}   // Type inferencu.
```

## Sets

```
let a = {string}         // Explicitly typed.
let a = {"hello"}        // Type inference.
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

Go code:

```go
type ResultString struct {
    Value *string
    Error error
}

var result = ResultString{Value: runtime.StringP("hello world")}

switch {
case result.Value != nil:
    value := *result.Value

case result.Error != nil:
    err := result.Error
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
    switch {
    case tuple.A == "a":
        n := tuple.B
        fmt.Sprintf("a %d", n)

    default:
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

## Interoperability with Go/C?

If the language is hosted by the Go runtime, should it support interoperability with Go? Or C?

Pros:
- large set of existing libraries

Cons:
- how does immutability interoperate with Go?
- limits the language to constructs supported by the Go runtime


## Examples
 
### Futures

```
class Future<T> {
    let notify = chan<bool>()
    let value: T?

    pub fn set(value: T) {
        self.value = value
        close(self.notify)
    }

    pub fn ready(): bool {
        if let ok = <-notify {
            return true
        }
        return false
    }

    // Blocking get of the future value.
    pub fn get(): T? {
        <-notify
        return value
    }
}
```

