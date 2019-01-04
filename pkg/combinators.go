package pkg

import (
	"github.com/mattfenwick/gunparse/pkg/maybeerror"
)

// Parser ...
type Parser struct {
	Parse func(xs interface{}, s interface{}) *maybeerror.MaybeError
}

// NewParser ...
func NewParser(parse func(xs interface{}, s interface{}) *maybeerror.MaybeError) *Parser {
	return &Parser{Parse: parse}
}

func newParser(f func(xs interface{}, s interface{}) interface{}) *Parser {
	g := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		return f(xs, s).(*maybeerror.MaybeError)
	}
	return NewParser(g)
}

// ParseResult ...
type ParseResult struct {
	Result interface{}
	Rest   interface{}
	State  interface{}
}

func result(value interface{}, rest interface{}, state interface{}) *ParseResult {
	return &ParseResult{Result: value, Rest: rest, State: state}
}

func good(value interface{}, rest interface{}, state interface{}) *maybeerror.MaybeError {
	return maybeerror.Pure(result(value, rest, state))
}

// Pure ...
func Pure(x interface{}) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		return good(x, xs, s)
	}
	return NewParser(f)
}

// Zero ...
var Zero = NewParser(func(xs interface{}, s interface{}) *maybeerror.MaybeError { return maybeerror.Zero })

// Error ...
func Error(e interface{}) *Parser {
	return NewParser(func(xs interface{}, s interface{}) *maybeerror.MaybeError { return maybeerror.Error(e) })
}

// Fmap ...
func Fmap(g func(interface{}) interface{}, parser *Parser) *Parser {
	h := func(pi interface{}) interface{} {
		p := pi.(*ParseResult)
		return &ParseResult{Result: g(p.Result), Rest: p.Rest, State: p.State}
	}
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		return parser.Parse(xs, s).Fmap(h)
	}
	return NewParser(f)
}

// Bind ...
func Bind(parser *Parser, g func(interface{}) *Parser) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		r := parser.Parse(xs, s)
		if r.Status == maybeerror.StatusSuccess {
			val := r.Value.(*ParseResult)
			return g(val.Result).Parse(val.Rest, val.State)
		}
		return r
	}
	return NewParser(f)
}

// Check ...
func Check(predicate func(interface{}) bool, parser *Parser) *Parser {
	return Bind(parser, func(value interface{}) *Parser {
		truth := value.(bool)
		if truth {
			return Pure(value)
		}
		return Zero
	})
}

// Update ...
func Update(f func(interface{}) interface{}) *Parser {
	g := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		ys := f(xs)
		return good(ys, ys, s)
	}
	return NewParser(g)
}

// Get is a Parser e s (m t) (m t)
var Get = Update(id)

// Put is a m t -> Parser e s (m t) a
// can't use `var Put = compose(Update, constF)` because of type system limitations
func Put(xs interface{}) *Parser {
	f := func(ys interface{}, s interface{}) *maybeerror.MaybeError {
		return good(xs, xs, s)
	}
	return NewParser(f)
}

func UpdateState(g func(interface{}) interface{}) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		newState := g(s)
		return good(newState, xs, newState)
	}
	return NewParser(f)
}

// GetState is a Parser e s (m t) s
var GetState = UpdateState(id)

// PutState is a s -> Parser e s (m t) a
func PutState(s interface{}) *Parser {
	g := func(xs interface{}, _ interface{}) *maybeerror.MaybeError {
		return good(s, xs, s)
	}
	return NewParser(g)
}

func Many0(parser *Parser) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		vals := []interface{}{}
		tokens := xs
		state := s
		for {
			r := parser.Parse(tokens, state)
			if r.Status == maybeerror.StatusSuccess {
				result := r.Value.(*ParseResult)
				vals = append(vals, result.Result)
				state = result.State
				tokens = result.Rest
			} else if r.Status == maybeerror.StatusFailure {
				return good(vals, tokens, state)
			} else {
				return r
			}
		}
	}
	return NewParser(f)
}

func Many1(parser *Parser) *Parser {
	f := func(value interface{}) bool {
		items := value.([]interface{})
		return len(items) > 0
	}
	return Check(f, Many0(parser))
}

func Seq(parsers []*Parser) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		vals := []interface{}{}
		state := s
		tokens := xs
		for _, p := range parsers {
			r := p.Parse(tokens, state)
			if r.Status == maybeerror.StatusSuccess {
				result := r.Value.(*ParseResult)
				vals = append(vals, result.Result)
				state = result.State
				tokens = result.Rest
			} else {
				return r
			}
		}
		return good(vals, tokens, state)
	}
	return NewParser(f)
}

func AppP(p *Parser, parsers []*Parser) *Parser {
	g := func(fi interface{}) *Parser {
		f := fi.(func(interface{}) interface{})
		return Fmap(func(args interface{}) interface{} { return f(args) }, Seq(parsers))
	}
	return Bind(p, g)
}

func App(f func(interface{}) interface{}, parsers []*Parser) *Parser {
	return AppP(Pure(f), parsers)
}

func Seq2L(p1 *Parser, p2 *Parser) *Parser {
	f := func(vals interface{}) interface{} {
		return vals.([]interface{})[0]
	}
	return App(f, []*Parser{p1, p2})
}

func Seq2R(p1 *Parser, p2 *Parser) *Parser {
	f := func(vals interface{}) interface{} {
		return vals.([]interface{})[1]
	}
	return App(f, []*Parser{p1, p2})
}

func Repeat(count int, parser *Parser) *Parser {
	parsers := make([]*Parser, count)
	for i := 0; i < count; i++ {
		parsers[i] = parser
	}
	return Seq(parsers)
}

func Lookahead(parser *Parser) *Parser {
	g := func(xs interface{}) *Parser {
		h := func(s interface{}) *Parser {
			f := func(a interface{}) interface{} {
				return a.([]interface{})[0]
			}
			return App(f, []*Parser{parser, Put(xs), PutState(s)})
		}
		return Bind(GetState, h)
	}
	return Bind(Get, g)
}

func Not0(parser *Parser) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		r := parser.Parse(xs, s)
		switch r.Status {
		case maybeerror.StatusError:
			return r
		case maybeerror.StatusSuccess:
			return maybeerror.Zero
		default:
			return good(nil, xs, s)
		}
	}
	return NewParser(f)
}

func Alt(parsers []*Parser) *Parser {
	f := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		r := maybeerror.Zero
		for _, p := range parsers {
			r = p.Parse(xs, s)
			if r.Status != maybeerror.StatusFailure {
				return r
			}
		}
		return r
	}
	return NewParser(f)
}

func Optional(parser *Parser, defaultValue interface{}) *Parser {
	return Alt([]*Parser{parser, Pure(defaultValue)})
}

func CatchError(parser *Parser, f func(interface{}) *Parser) *Parser {
	g := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		v := parser.Parse(xs, s)
		if v.Status == maybeerror.StatusError {
			return f(v.Value).Parse(xs, s)
		}
		return v
	}
	return NewParser(g)
}

func MapError(f func(interface{}) interface{}, parser *Parser) *Parser {
	g := func(e interface{}) *Parser {
		return Error(f(e))
	}
	return CatchError(parser, g)
}

// Commit: e -> Parser e s (m t) a -> Parser e s (m t) a
func Commit(e interface{}, parser *Parser) *Parser {
	return Alt([]*Parser{parser, Error(e)})
}

func AddError(e interface{}, parser *Parser) *Parser {
	f := func(es interface{}) interface{} {
		return append([]interface{}{e}, es.([]interface{}))
	}
	return MapError(f, parser)
}

// SepBy1     Parser e s (m t) a -> Parser e s (m t) b -> Parser e s (m t) (a, [(b, a)])
func SepBy1(parser *Parser, separator *Parser) *Parser {
	return App(id, []*Parser{parser, Many0(App(id, []*Parser{separator, parser}))})
}

// SepBy0  Parser e s (m t) a -> Parser e s (m t) b -> Parser e s (m t) (Maybe (a, [(b, a)]))
func SepBy0(parser *Parser, separator *Parser) *Parser {
	return Optional(SepBy1(parser, separator), nil)
}

type Itemizer struct {
	F    func(interface{}, interface{}) interface{}
	Item *Parser
}

func NewItemizer(f func(interface{}, interface{}) interface{}) *Itemizer {
	it := &Itemizer{F: f}
	it.Item = it.item()
	return it
}

func (it *Itemizer) item() *Parser {
	g := func(xs interface{}, s interface{}) *maybeerror.MaybeError {
		ys := xs.([]interface{})
		if len(ys) == 0 {
			return maybeerror.Zero
		}
		first := ys[0]
		rest := ys[1:]
		return good(first, rest, it.F(first, s))
	}
	return NewParser(g)
}

/*
class Itemizer(object):

    def satisfy(self, pred):
        '''
        (t -> Bool) -> Parser e s (m t) t
        '''
        checkFunction('satisfy', pred)
        return check(pred, self.item)

    def literal(self, x):
        '''
        Eq t => t -> Parser e s (m t) t
        '''
        return self.satisfy(lambda y: x == y)

    def not1(self, parser):
        '''
        Parser e s (m t) a -> Parser e s (m t) t
        '''
        checkParser('not1', parser)
        return seq2R(not0(parser), self.item)

    def string(self, elems):
        '''
        Eq t => [t] -> Parser e s (m t) [t]
        '''
        matcher = seq(list(map(self.literal, elems)))
        return seq2R(matcher, pure(elems))

    def oneOf(self, elems):
        c_set = set(elems)
        return self.satisfy(lambda x: x in c_set)


# doesn't do anything to the state
basic    = Itemizer(functions.second)
# assumes the state is a 2-tuple of integers (line, column)
position = Itemizer(functions.updatePosition)
# assumes that state is an integer -- how many tokens have been consumed
count    = Itemizer(lambda _, s: s + 1)


def run(parser, input_string, state=(1,1)):
    '''
    Run a parser given the token input and state.
    '''
    return parser.parse(ConsList(input_string), state)
*/
