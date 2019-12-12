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

public struct Vector {
    public let x, y, z float32 = (0, 0, 0)

    public fn length() float32 { // Pure.
        return Math.sqrt(x * x + y * y + z * z)
    }

    public fn add(other Vector) { // Impure.
        x += other.x
        y += other.y
        z += \
			other.z

		fn closure() {
			println(other.x)
		}

		let v = Vector{x: 1}
	
		if x > 10 {
			x = 10
		} else {
			x = x
		}
	}
}

let origin = Vector{x: 0, y: 0, z: 0,}
	
alias Point Vector
	
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
	let result = Result.value("hello world")

	switch (result) {
	case value(v):
		println(v)

	case error(e):
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
	ast, err := ParseString(testSource)
	repr.Println(ast)
	require.NoError(t, err)
}
