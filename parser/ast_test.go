package parser

import (
	"fmt"
	"testing"

	"github.com/alecthomas/repr"
	"github.com/stretchr/testify/require"
)

// This source should contain all constructs supported by the parser.
const testSource = `
import "os"

pub class Vector {
    pub let x, y, z: float = (0, 0, 0)

	init(x, y, z: float) {
		self.x = x
		self.y = y
		self.z = z
	}

    override pub fn length(): float { // Pure.
        return Math.sqrt(x * x + y * y + z * z)
    }

    pub fn add(other: Vector) { // Impure.
        x += other.x
        y += other.y
        z += \
			other.z

		fn closure() {
			println(other.x)
		}

		let v = Vector(1, 2, 3)
	
		if x > 10 {
			x = 10
			closure()
		} else {
			x = x
		}
	}
}

let origin = Vector()
	
enum Result<T> {
    case value(T)
    case error(error)
}

enum Option<T> {
    case value(T)
    case none
	
	fn which() {
	}
}
	
fn test() {
	let dict = new {string: int}()
	let array = {string}()
	let result = Result.value("hello world")

	for v in result {
	}

	switch result {
	case .value(v):
		println(v)

	case .error(e):
		panic(e)
	}
}
`

func BenchmarkParse(b *testing.B) {
	fmt.Println(len(testSource))
	for i := 0; i < b.N; i++ {
		_, err := ParseString(testSource)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func TestParse(t *testing.T) {
	tests := []struct {
		name   string
		source string
		fail   string
		debug  bool
	}{
		{name: "ArrayLiteral",
			source: `
				fn f(a: [int]) {}

				let a = [1, 2, 3]
			`},
		{name: "SetLiteral",
			source: `
				fn f(a: {int}) {}

				let a = f({1, 2, 3})
			`},
		{name: "DictLiteral",
			source: `
				fn f(a:{int:[int]}) {}

				let a = f({1:[2], 2:[3], 3:[4]})
			`},
		{name: "ComplexReference",
			source: `
				let a = b[10].c()
				`},
		// TODO: Fails with: 3:4: unexpected token "<EOF>" (expected ";")
		// {name: "GenericReference",
		// 	source: `
		// 		let a: Pair<string, int>
		// 	`,
		// },
		{name: "OptionalValue",
			source: `
				let a: string? = "hello"
			`,
		},
		{name: "CompoundEnumCases",
			source: `
				// Represents any JSON value.
				enum ResolvedValue {
					case Number(float)
					case String(string)
					case List([ResolvedValue])
					case Object({string: ResolvedValue})
				}
			`},
		{name: "AnonymousEnum",
			source: `fn f(): string|int {}`},
		{name: "InterpolatedString",
			source: `
				let a = "Hello {user + "{name()}"}, how are you?"
			`},
		{name: "FullSource",
			source: testSource},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, err := ParseString(test.source)
			if test.debug {
				repr.Println(ast)
			}
			if test.fail != "" {
				require.EqualError(t, err, test.fail)
			} else {
				require.NoError(t, err, repr.String(ast, repr.Indent("  ")))
			}
		})
	}
}
