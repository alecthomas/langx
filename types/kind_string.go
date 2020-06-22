// Code generated by "stringer -linecomment -type Kind"; DO NOT EDIT.

package types

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[KindNone-0]
	_ = x[KindGeneric-1]
	_ = x[KindFunc-2]
	_ = x[KindLiteralInt-3]
	_ = x[KindLiteralFloat-4]
	_ = x[KindLiteralString-5]
	_ = x[KindString-6]
	_ = x[KindBool-7]
	_ = x[KindInt-8]
	_ = x[KindFloat-9]
	_ = x[KindTuple-10]
	_ = x[KindClass-11]
	_ = x[KindEnum-12]
	_ = x[KindCase-13]
	_ = x[KindAlias-14]
	_ = x[KindAny-15]
	_ = x[KindInterface-16]
}

const _Kind_name = "nonegenericfunctionliteral intliteral floatliteral stringstringboolintfloattupleclassenumcasealiasanyinterface"

var _Kind_index = [...]uint8{0, 4, 11, 19, 30, 43, 57, 63, 67, 70, 75, 80, 85, 89, 93, 98, 101, 110}

func (i Kind) String() string {
	if i < 0 || i >= Kind(len(_Kind_index)-1) {
		return "Kind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}
