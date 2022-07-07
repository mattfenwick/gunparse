package example

import (
	"github.com/mattfenwick/gunparse/pkg"
	"github.com/mattfenwick/gunparse/pkg/utils"
)

type ErrorFrame = pkg.Pair[string, *pkg.Pair[int, int]]
type ParseError = []*ErrorFrame

func NewErrorFrame(message string, line int, col int) *pkg.Pair[string, *pkg.Pair[int, int]] {
	return &pkg.Pair[string, *pkg.Pair[int, int]]{
		A: message,
		B: &pkg.Pair[int, int]{
			A: line,
			B: col,
		},
	}
}

var (
	itemizer = pkg.PositionItemizer[ParseError]()

	item    = itemizer.Item
	literal = itemizer.Literal
	satisfy = itemizer.Satisfy
)

func oneOf(s string) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, rune] {
	return itemizer.OneOf(stringToRunes(s))
}

func matchString(s string) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, []rune] {
	return itemizer.MatchString(stringToRunes(s))
}

func not1[A any](p *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, A]) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, rune] {
	return pkg.Not1[ParseError, *pkg.Pair[int, int], rune, A](itemizer, p)
}

func stringToRunes(s string) []rune {
	var out []rune
	for i := 0; i < len(s); i++ {
		out = append(out, rune(s[i]))
	}
	return out
}

var (
	whitespace = pkg.Many0(oneOf(" \t\n\r"))

	_digits = pkg.Many0(oneOf("0123456789"))

	_decimal = pkg.App2(NewDecimal, literal('.'), pkg.Cut("digits", _digits))

	_exponent = pkg.App3(NewExponent,
		oneOf("eE"),
		pkg.Optional(oneOf("+-"), '+'), // TODO losing CST information by inserting default '+'
		pkg.Cut("power", _digits))

	_number_1 = pkg.App4(NewNumber,
		literal('-'),
		pkg.Cut("digits", _digits),
		pkg.Optional(_decimal, nil),
		pkg.Optional(_exponent, nil))

	_number_2 = pkg.App4(NewNumber,
		pkg.Pure[ParseError, *pkg.Pair[int, int], rune]('+'),
		_digits,
		pkg.Optional(_decimal, nil),
		pkg.Optional(_exponent, nil))

	// there are two number patterns solely to get the error reporting right
	//   if there's a `-` but a number can't be parsed, that's an error
	_number = pkg.AltSplat(_number_1, _number_2)

	_char = pkg.App(NewCharacter, not1(oneOf("\\\"")))

	// this allows *any* character to be escaped
	//   invalid characters are handled by a later pass
	//   this assumes that doing so will not change the
	//   overall structure of the parse result
	_escape = pkg.App2(NewEscape,
		literal('\\'),
		item)

	_hexC = oneOf("0123456789abcdefABCDEF")

	_unic = pkg.App2(NewUnicodeEscape,
		matchString("\\u"),
		pkg.Cut("4 hexadecimal digits", pkg.Repeat(4, _hexC)))

	_nilChar          = pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Character](nil)
	_nilEscape        = pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Escape](nil)
	_nilUnicodeEscape = pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *UnicodeEscape](nil)

	_jsonStringChar = pkg.AltSplat[ParseError, *pkg.Pair[int, int], rune, *JsonStringChar](
		pkg.App3(NewJsonStringChar, _char, _nilEscape, _nilUnicodeEscape),
		pkg.App3(NewJsonStringChar, _nilChar, _nilEscape, _unic),
		pkg.App3(NewJsonStringChar, _nilChar, _escape, _nilUnicodeEscape))

	_jsonString = pkg.App3(NewJsonString,
		literal('"'),
		pkg.Many0(_jsonStringChar),
		pkg.Cut("double-quote", literal('"')))

	_keyword = pkg.App(NewKeyword,
		pkg.Alt(utils.MapList(matchString, []string{"true", "false", "null"})))
)

func token[A any](p *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, A]) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, A] {
	return pkg.Seq2L(p, whitespace)
}

var (
	jsonString = token(_jsonString)
	number     = token(_number)
	keyword    = token(_keyword)

	os    = token(literal('['))
	cs    = token(literal(']'))
	oc    = token(literal('{'))
	cc    = token(literal('}'))
	comma = token(literal(','))
	colon = token(literal(':'))
)

var (
	object     *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *Object]     = nil
	array      *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *Array]      = nil
	value      *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *JsonValue]  = nil
	keyValPair *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *KeyValPair] = nil
	json       *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *JsonValue]  = nil
)

func init() {
	// a hack to allow mutual recursion of rules
	object = pkg.NewError[ParseError, *pkg.Pair[int, int], rune, *Object]([]*ErrorFrame{NewErrorFrame("unimplemented", -1, -1)})
	array = pkg.NewError[ParseError, *pkg.Pair[int, int], rune, *Array]([]*ErrorFrame{NewErrorFrame("unimplemented", -1, -1)})

	_nilJsonString := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *JsonString](nil)
	_nilNumber := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Number](nil)
	_nilKeyword := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Keyword](nil)
	_nilObject := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Object](nil)
	_nilArray := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Array](nil)

	value = pkg.AltSplat(
		pkg.App5(NewJsonValue, jsonString, _nilNumber, _nilKeyword, _nilObject, _nilArray),
		pkg.App5(NewJsonValue, _nilJsonString, number, _nilKeyword, _nilObject, _nilArray),
		pkg.App5(NewJsonValue, _nilJsonString, _nilNumber, keyword, _nilObject, _nilArray),
		pkg.App5(NewJsonValue, _nilJsonString, _nilNumber, _nilKeyword, object, _nilArray),
		pkg.App5(NewJsonValue, _nilJsonString, _nilNumber, _nilKeyword, _nilObject, array))

	array.Parse = pkg.App3(NewArray,
		os,
		pkg.App(func(s *pkg.SepByResult[*JsonValue, rune]) []*JsonValue {
			return s.Values()
		}, pkg.SepBy0(value, comma)),
		pkg.Cut("close", cs)).Parse

	keyValPair = pkg.App3(NewKeyValPair,
		jsonString,
		pkg.Cut("colon", colon),
		pkg.Cut("value", value))

	object.Parse = pkg.App3(NewObject,
		oc,
		pkg.App(func(s *pkg.SepByResult[*KeyValPair, rune]) []*KeyValPair {
			return s.Values()
		}, pkg.SepBy0(keyValPair, comma)),
		pkg.Cut("close", cc)).Parse

	json = pkg.Seq2L(
		pkg.Seq2R(whitespace, pkg.Cut("json value", value)),
		pkg.Cut("unparsed input remaining", pkg.Not0(item)))
}
