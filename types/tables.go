package types

import (
	"github.com/alecthomas/langx/parser"
)

type opKey struct {
	Left  Kind
	Op    Op
	Right Kind
}

// Table of allowed operations.
var opMap = func() map[opKey]bool {
	out := map[opKey]bool{
		// Booleans.
		{KindBool, parser.OpNot, KindNone}: true, // !<num>

		// Strings.
		{KindString, parser.OpAdd, KindString}:     true,
		{KindString, parser.OpAddAsgn, KindString}: true,

		{KindString, parser.OpEq, KindString}: true,
		{KindString, parser.OpNe, KindString}: true,
		{KindString, parser.OpGe, KindString}: true,
		{KindString, parser.OpGt, KindString}: true,
		{KindString, parser.OpLe, KindString}: true,
		{KindString, parser.OpLt, KindString}: true,
	}
	ops := []parser.Op{
		parser.OpEq,
		parser.OpNe,
		parser.OpGe,
		parser.OpGt,
		parser.OpLe,
		parser.OpLt,
		parser.OpAdd,
		parser.OpSub,
		parser.OpDiv,
		parser.OpMul,
		parser.OpMod,
		parser.OpAsgn,
		parser.OpAddAsgn,
		parser.OpSubAsgn,
		parser.OpMulAsgn,
		parser.OpModAsgn,
	}
	for _, op := range ops {
		out[opKey{KindInt, op, KindInt}] = true
		out[opKey{KindInt, op, KindNumber}] = true
		out[opKey{KindNumber, op, KindInt}] = true
		out[opKey{KindFloat, op, KindFloat}] = true
		out[opKey{KindFloat, op, KindNumber}] = true
		out[opKey{KindNumber, op, KindFloat}] = true
	}
	return out
}()

type coercionKey struct {
	From Kind
	To   Kind
}

var coercionMap = map[coercionKey]bool{
	{KindNumber, KindInt}:   true,
	{KindNumber, KindFloat}: true,
	{KindFloat, KindInt}:    true,
	{KindInt, KindFloat}:    true,
}
