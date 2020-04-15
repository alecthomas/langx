package parser

import (
	"fmt"
)

//go:generate stringer -linecomment -type Op

type Op int

const (
	OpNone    Op = iota //
	OpAsgn              // =
	OpAddAsgn           // +=
	OpSubAsgn           // -=
	OpMulAsgn           // *=
	OpDivAsgn           // /=
	OpModAsgn           // %=
	OpPowAsgn           // ^=
	OpGe                // >=
	OpLe                // <=
	OpAnd               // &&
	OpOr                // ||
	OpEq                // ==
	OpNe                // !=
	OpSub               // -
	OpAdd               // +
	OpMul               // *
	OpDiv               // /
	OpLt                // <
	OpGt                // >
	OpMod               // %
	OpPow               // ^
	OpNot               // !
	OpSend              // ->
	OpBitOr             // |
	OpBitAnd            // &
)

func (o Op) GoString() string {
	switch o {
	case OpAsgn:
		return "parser.OpAsgn"
	case OpModAsgn:
		return "parser.OpModAssgn"
	case OpAddAsgn:
		return "parser.OpAddAsgn"
	case OpSubAsgn:
		return "parser.OpSubAsgn"
	case OpMulAsgn:
		return "parser.OpMulAsgn"
	case OpDivAsgn:
		return "parser.OpDivAsgn"
	case OpPowAsgn:
		return "parser.OpPowAsgn"
	case OpEq:
		return "parser.OpEq"
	case OpNe:
		return "parser.OpNe"
	case OpGe:
		return "parser.OpGe"
	case OpLe:
		return "parser.OpLe"
	case OpAnd:
		return "parser.OpAnd"
	case OpOr:
		return "parser.OpOr"
	case OpSub:
		return "parser.OpSub"
	case OpAdd:
		return "parser.OpAdd"
	case OpMul:
		return "parser.OpMul"
	case OpDiv:
		return "parser.OpDiv"
	case OpLt:
		return "parser.OpLt"
	case OpGt:
		return "parser.OpGt"
	case OpMod:
		return "parser.OpMod"
	case OpPow:
		return "parser.OpPow"
	case OpNot:
		return "parser.OpNot"
	case OpSend:
		return "parser.OpSend"
	case OpBitAnd:
		return "parser.OpBitAnd"
	case OpBitOr:
		return "parser.OpBitOr"
	default:
		panic("??")
	}
}

func (o *Op) Capture(values []string) error {
	switch values[0] {
	case "%=":
		*o = OpModAsgn
	case "+=":
		*o = OpAddAsgn
	case "-=":
		*o = OpSubAsgn
	case "/=":
		*o = OpDivAsgn
	case "*=":
		*o = OpMulAsgn
	case "^=":
		*o = OpPowAsgn
	case "=":
		*o = OpAsgn
	case ">=":
		*o = OpGe
	case "<=":
		*o = OpLe
	case "&&":
		*o = OpAnd
	case "||":
		*o = OpOr
	case "==":
		*o = OpEq
	case "!=":
		*o = OpNe
	case "-":
		*o = OpSub
	case "+":
		*o = OpAdd
	case "*":
		*o = OpMul
	case "/":
		*o = OpDiv
	case "%":
		*o = OpMod
	case "<":
		*o = OpLt
	case ">":
		*o = OpGt
	case "^":
		*o = OpPow
	case "!":
		*o = OpNot
	case "|":
		*o = OpBitOr
	case "&":
		*o = OpBitAnd
	default:
		return fmt.Errorf("invalid expression operator %q", values[0])
	}
	return nil
}
