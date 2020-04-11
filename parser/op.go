package parser

//go:generate stringer -linecomment -type Op

type Op int

const (
	OpNone    Op = iota //
	OpModAsgn           // %=
	OpGe                // >=
	OpLe                // <=
	OpAnd               // &&
	OpOr                // ||
	OpEq                // ==
	OpNe                // !=
	OpAddAsgn           // +=
	OpSubAsgn           // -=
	OpMulAsgn           // *=
	OpDivAsgn           // /=
	OpPowAsgn           // ^=
	OpSub               // -
	OpAsgn              // =
	OpAdd               // +
	OpMul               // *
	OpDiv               // /
	OpLt                // <
	OpGt                // >
	OpMod               // %
	OpPow               // ^
	OpNot               // !
	OpSend              // ->
)

func (o Op) GoString() string {
	switch o {
	case OpModAsgn:
		return "parser.OpModAssgn"
	case OpGe:
		return "parser.OpGe"
	case OpLe:
		return "parser.OpLe"
	case OpAnd:
		return "parser.OpAnd"
	case OpOr:
		return "parser.OpOr"
	case OpEq:
		return "parser.OpEq"
	case OpNe:
		return "parser.OpNe"
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
	case OpSub:
		return "parser.OpSub"
	case OpAsgn:
		return "parser.OpAsgn"
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
	default:
		panic("??")
	}
}

func (o *Op) Capture(values []string) error {
	switch values[0] {
	case "%=":
		*o = OpModAsgn
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
	default:
		panic(values[0])
	}
	return nil
}

type opInfo struct {
	RightAssociative bool
	Priority         int
}

var info = map[Op]opInfo{
	OpAdd: {Priority: 1},
	OpSub: {Priority: 1},
	OpMul: {Priority: 2},
	OpDiv: {Priority: 2},
	OpMod: {Priority: 2},
	OpPow: {RightAssociative: true, Priority: 3},
}
