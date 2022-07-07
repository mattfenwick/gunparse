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
