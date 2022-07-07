package example

type Decimal struct {
	Dot    rune
	Digits []rune
}

func NewDecimal(dot rune, digits []rune) *Decimal {
	return &Decimal{
		Dot:    dot,
		Digits: digits,
	}
}

type Exponent struct {
	Letter rune
	Sign   rune
	Power  []rune
}

func NewExponent(letter rune, sign rune, power []rune) *Exponent {
	return &Exponent{
		Letter: letter,
		Sign:   sign,
		Power:  power,
	}
}

type Number struct {
	Sign     rune
	Integer  []rune
	Decimal  *Decimal
	Exponent *Exponent
}

func NewNumber(sign rune, integer []rune, decimal *Decimal, exponent *Exponent) *Number {
	return &Number{
		Sign:     sign,
		Integer:  integer,
		Decimal:  decimal,
		Exponent: exponent,
	}
}

type Character struct {
	Value rune
}

func NewCharacter(value rune) *Character {
	return &Character{
		Value: value,
	}
}

type Escape struct {
	Open  rune
	Value rune
}

func NewEscape(open rune, value rune) *Escape {
	return &Escape{
		Open:  open,
		Value: value,
	}
}

type UnicodeEscape struct {
	Open  []rune
	Value []rune
}

func NewUnicodeEscape(open []rune, value []rune) *UnicodeEscape {
	return &UnicodeEscape{
		Open:  open,
		Value: value,
	}
}

type JsonStringChar struct {
	Char          *Character
	Escape        *Escape
	UnicodeEscape *UnicodeEscape
}

func NewJsonStringChar(char *Character, escape *Escape, unicodeescape *UnicodeEscape) *JsonStringChar {
	return &JsonStringChar{
		Char:          char,
		Escape:        escape,
		UnicodeEscape: unicodeescape,
	}
}

type JsonString struct {
	Open  rune
	Value []*JsonStringChar
	Close rune
}

func NewJsonString(open rune, value []*JsonStringChar, close rune) *JsonString {
	return &JsonString{
		Open:  open,
		Value: value,
		Close: close,
	}
}
