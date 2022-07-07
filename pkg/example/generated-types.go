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
	Open  string
	Value []rune
}

func NewUnicodeEscape(open string, value []rune) *UnicodeEscape {
	return &UnicodeEscape{
		Open:  open,
		Value: value,
	}
}

type StringChar struct {
	Char          *Character
	Escape        *Escape
	UnicodeEscape *UnicodeEscape
}

func NewStringChar(char *Character, escape *Escape, unicodeescape *UnicodeEscape) *StringChar {
	return &StringChar{
		Char:          char,
		Escape:        escape,
		UnicodeEscape: unicodeescape,
	}
}

type String struct {
	Open  rune
	Value []*StringChar
	Close rune
}

func NewString(open rune, value []*StringChar, close rune) *String {
	return &String{
		Open:  open,
		Value: value,
		Close: close,
	}
}

type Keyword struct {
	Value string
}

func NewKeyword(value string) *Keyword {
	return &Keyword{
		Value: value,
	}
}

type KeyValPair struct {
	Key   *String
	Colon rune
	Value *JsonValue
}

func NewKeyValPair(key *String, colon rune, value *JsonValue) *KeyValPair {
	return &KeyValPair{
		Key:   key,
		Colon: colon,
		Value: value,
	}
}

type Array struct {
	Open  rune
	Body  []*JsonValue
	Close rune
}

func NewArray(open rune, body []*JsonValue, close rune) *Array {
	return &Array{
		Open:  open,
		Body:  body,
		Close: close,
	}
}

type Object struct {
	Open  rune
	Body  []*KeyValPair
	Close rune
}

func NewObject(open rune, body []*KeyValPair, close rune) *Object {
	return &Object{
		Open:  open,
		Body:  body,
		Close: close,
	}
}

type JsonValue struct {
	String  *String
	Number  *Number
	Keyword *Keyword
	Object  *Object
	Array   *Array
}

func NewJsonValue(string *String, number *Number, keyword *Keyword, object *Object, array *Array) *JsonValue {
	return &JsonValue{
		String:  string,
		Number:  number,
		Keyword: keyword,
		Object:  object,
		Array:   array,
	}
}
