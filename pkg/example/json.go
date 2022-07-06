package example

import "github.com/mattfenwick/gunparse/pkg"

type ParseError = []string

var (
	itemizer = pkg.PositionItemizer[ParseError]()

	item        = itemizer.Item
	literal     = itemizer.Literal
	satisfy     = itemizer.Satisfy
	oneOf       = itemizer.OneOf
	matchString = itemizer.MatchString
)

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
	whitespace = pkg.Many0(oneOf(stringToRunes(" \t\n\r")))

	_digits = pkg.Many0(oneOf(stringToRunes("0123456789")))

	// TODO need cst help ?
)
