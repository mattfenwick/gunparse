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
	return itemizer.OneOf(StringToRunes(s))
}

func matchString(s string) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, []rune] {
	return itemizer.MatchString(StringToRunes(s))
}

func not1[A any](p *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, A]) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, rune] {
	return pkg.Not1[ParseError, *pkg.Pair[int, int], rune, A](itemizer, p)
}

func StringToRunes(s string) []rune {
	var out []rune
	for i := 0; i < len(s); i++ {
		out = append(out, rune(s[i]))
	}
	return out
}

var (
	Whitespace = pkg.Many0(oneOf(" \t\n\r"))

	PrivateDigits = pkg.Many0(oneOf("0123456789"))

	PrivateDecimal = pkg.App2(NewDecimal, literal('.'), pkg.Cut("digits", PrivateDigits))

	PrivateExponent = pkg.App3(NewExponent,
		oneOf("eE"),
		pkg.Optional(oneOf("+-"), '+'), // TODO losing CST information by inserting default '+'
		pkg.Cut("power", PrivateDigits))

	PrivateNumber_1 = pkg.App4(NewNumber,
		literal('-'),
		pkg.Cut("digits", PrivateDigits),
		pkg.Optional(PrivateDecimal, nil),
		pkg.Optional(PrivateExponent, nil))

	PrivateNumber_2 = pkg.App4(NewNumber,
		pkg.Pure[ParseError, *pkg.Pair[int, int], rune]('+'),
		PrivateDigits,
		pkg.Optional(PrivateDecimal, nil),
		pkg.Optional(PrivateExponent, nil))

	// there are two number patterns solely to get the error reporting right
	//   if there's a `-` but a number can't be parsed, that's an error
	PrivateNumber = pkg.AltSplat(PrivateNumber_1, PrivateNumber_2)

	PrivateChar = pkg.App(NewCharacter, not1(oneOf("\\\"")))

	// this allows *any* character to be escaped
	//   invalid characters are handled by a later pass
	//   this assumes that doing so will not change the
	//   overall structure of the parse result
	PrivateEscape = pkg.App2(NewEscape,
		literal('\\'),
		item)

	PrivateHexC = oneOf("0123456789abcdefABCDEF")

	PrivateUnicode = pkg.App2(NewUnicodeEscape,
		matchString("\\u"),
		pkg.Cut("4 hexadecimal digits", pkg.Repeat(4, PrivateHexC)))

	_nilChar          = pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Character](nil)
	_nilEscape        = pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Escape](nil)
	_nilUnicodeEscape = pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *UnicodeEscape](nil)

	PrivateJsonStringChar = pkg.AltSplat[ParseError, *pkg.Pair[int, int], rune, *StringChar](
		pkg.App3(NewStringChar, PrivateChar, _nilEscape, _nilUnicodeEscape),
		pkg.App3(NewStringChar, _nilChar, _nilEscape, PrivateUnicode),
		pkg.App3(NewStringChar, _nilChar, PrivateEscape, _nilUnicodeEscape))

	PrivateJsonString = pkg.App3(NewString,
		literal('"'),
		pkg.Many0(PrivateJsonStringChar),
		pkg.Cut("double-quote", literal('"')))

	PrivateKeyword = pkg.App(NewKeyword,
		pkg.Alt(utils.MapList(matchString, []string{"true", "false", "null"})))
)

func token[A any](p *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, A]) *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, A] {
	return pkg.Seq2L(p, Whitespace)
}

var (
	StringParser  = token(PrivateJsonString)
	NumberParser  = token(PrivateNumber)
	KeywordParser = token(PrivateKeyword)

	os    = token(literal('['))
	cs    = token(literal(']'))
	oc    = token(literal('{'))
	cc    = token(literal('}'))
	comma = token(literal(','))
	colon = token(literal(':'))
)

var (
	ObjectParser     *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *Object]     = nil
	ArrayParser      *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *Array]      = nil
	ValueParser      *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *JsonValue]  = nil
	KeyValPairParser *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *KeyValPair] = nil
	JsonParser       *pkg.Parser[ParseError, *pkg.Pair[int, int], rune, *JsonValue]  = nil
)

func init() {
	// a hack to allow mutual recursion of rules
	ObjectParser = pkg.NewError[ParseError, *pkg.Pair[int, int], rune, *Object]([]*ErrorFrame{NewErrorFrame("unimplemented", -1, -1)})
	ArrayParser = pkg.NewError[ParseError, *pkg.Pair[int, int], rune, *Array]([]*ErrorFrame{NewErrorFrame("unimplemented", -1, -1)})

	_nilString := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *String](nil)
	_nilNumber := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Number](nil)
	_nilKeyword := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Keyword](nil)
	_nilObject := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Object](nil)
	_nilArray := pkg.Pure[ParseError, *pkg.Pair[int, int], rune, *Array](nil)

	ValueParser = pkg.AltSplat(
		pkg.App5(NewJsonValue, StringParser, _nilNumber, _nilKeyword, _nilObject, _nilArray),
		pkg.App5(NewJsonValue, _nilString, NumberParser, _nilKeyword, _nilObject, _nilArray),
		pkg.App5(NewJsonValue, _nilString, _nilNumber, KeywordParser, _nilObject, _nilArray),
		pkg.App5(NewJsonValue, _nilString, _nilNumber, _nilKeyword, ObjectParser, _nilArray),
		pkg.App5(NewJsonValue, _nilString, _nilNumber, _nilKeyword, _nilObject, ArrayParser))

	ArrayParser.Parse = pkg.App3(NewArray,
		os,
		pkg.App(func(s *pkg.SepByResult[*JsonValue, rune]) []*JsonValue {
			return s.Values()
		}, pkg.SepBy0(ValueParser, comma)),
		pkg.Cut("close", cs)).Parse

	KeyValPairParser = pkg.App3(NewKeyValPair,
		StringParser,
		pkg.Cut("colon", colon),
		pkg.Cut("value", ValueParser))

	ObjectParser.Parse = pkg.App3(NewObject,
		oc,
		pkg.App(func(s *pkg.SepByResult[*KeyValPair, rune]) []*KeyValPair {
			return s.Values()
		}, pkg.SepBy0(KeyValPairParser, comma)),
		pkg.Cut("close", cc)).Parse

	JsonParser = pkg.Seq2L(
		pkg.Seq2R(Whitespace, pkg.Cut("json value", ValueParser)),
		pkg.Cut("unparsed input remaining", pkg.Not0(item)))
}
