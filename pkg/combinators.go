package pkg

import (
	"github.com/mattfenwick/gunparse/pkg/maybeerror"
)

// Unit represents the Haskell value `()` -- TODO delete?
//type Unit struct {}

type Parser[E, S, T, A any] struct {
	Parse func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]]
}

func NewParser[E, S, T, A any](parse func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]]) *Parser[E, S, T, A] {
	return &Parser[E, S, T, A]{Parse: parse}
}

func newParser[E, S, T, A any](f func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]]) *Parser[E, S, T, A] {
	g := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
		return f(xs, s)
	}
	return NewParser(g)
}

type ParseResult[T, S, A any] struct {
	Result A
	Rest   []T
	State  S
}

func result[T, S, A any](value A, rest []T, state S) *ParseResult[T, S, A] {
	return &ParseResult[T, S, A]{Result: value, Rest: rest, State: state}
}

func good[E, S, T, A any](value A, rest []T, state S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
	return maybeerror.NewSuccess[E, *ParseResult[T, S, A]](result(value, rest, state))
}

func Pure[E, S, T, A any](x A) *Parser[E, S, T, A] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
		return good[E, S, T, A](x, xs, s)
	}
	return NewParser(f)
}

func NewZero[E, S, T, A any]() *Parser[E, S, T, A] {
	return NewParser[E, S, T, A](func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
		return maybeerror.NewFailure[E, *ParseResult[T, S, A]]()
	})
}

func NewError[E, S, T, A any](e E) *Parser[E, S, T, A] {
	return NewParser[E, S, T, A](func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
		return maybeerror.NewError[E, *ParseResult[T, S, A]](e)
	})
}

func Fmap[E, S, T, A, B any](g func(A) B, parser *Parser[E, S, T, A]) *Parser[E, S, T, B] {
	h := func(p *ParseResult[T, S, A]) *ParseResult[T, S, B] {
		return &ParseResult[T, S, B]{Result: g(p.Result), Rest: p.Rest, State: p.State}
	}
	return NewParser[E, S, T, B](func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, B]] {
		return maybeerror.Fmap(parser.Parse(xs, s), h)
	})
}

func Bind[E, S, T, A, B any](parser *Parser[E, S, T, A], g func(A) *Parser[E, S, T, B]) *Parser[E, S, T, B] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, B]] {
		r := parser.Parse(xs, s)
		if r.Success != nil {
			val := r.Success.Value
			return g(val.Result).Parse(val.Rest, val.State)
		} else if r.Failure != nil {
			return maybeerror.NewFailure[E, *ParseResult[T, S, B]]()
		}
		return maybeerror.NewError[E, *ParseResult[T, S, B]](r.Error.Value)
	}
	return NewParser(f)
}

func Check[E, S, T, A any](predicate func(A) bool, parser *Parser[E, S, T, A]) *Parser[E, S, T, A] {
	return Bind[E, S, T, A](parser, func(value A) *Parser[E, S, T, A] {
		if predicate(value) {
			return Pure[E, S, T, A](value)
		}
		return NewZero[E, S, T, A]()
	})
}

func Update[E, S, T any](f func([]T) []T) *Parser[E, S, T, []T] {
	g := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, []T]] {
		ys := f(xs)
		return good[E, S, T, []T](ys, ys, s)
	}
	return NewParser(g)
}

func Get[E, S, T any]() *Parser[E, S, T, []T] {
	return Update[E, S, T](id[[]T])
}

func Put[E, S, T any](xs []T) *Parser[E, S, T, []T] {
	f := func(ys []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, []T]] {
		return good[E, S, T, []T](xs, xs, s)
	}
	return NewParser(f)
}

func UpdateState[E, S, T any](g func(S) S) *Parser[E, S, T, S] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, S]] {
		newState := g(s)
		return good[E, S, T, S](newState, xs, newState)
	}
	return NewParser(f)
}

func GetState[E, S, T any]() *Parser[E, S, T, S] {
	return UpdateState[E, S, T](id[S])
}

func PutState[E, S, T any](s2 S) *Parser[E, S, T, S] {
	f := func(ys []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, S]] {
		return good[E, S, T, S](s2, ys, s)
	}
	return NewParser(f)
}

func Many0[E, S, T, A any](parser *Parser[E, S, T, A]) *Parser[E, S, T, []A] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, []A]] {
		vals := []A{}
		tokens := xs
		state := s
		for {
			r := parser.Parse(tokens, state)
			if r.Success != nil {
				vals = append(vals, r.Success.Value.Result)
				state = r.Success.Value.State
				tokens = r.Success.Value.Rest
			} else if r.Failure != nil {
				return good[E, S, T, []A](vals, tokens, state)
			} else {
				return maybeerror.NewError[E, *ParseResult[T, S, []A]](r.Error.Value)
			}
		}
	}
	return NewParser(f)
}

func Many1[E, S, T, A any](parser *Parser[E, S, T, A]) *Parser[E, S, T, []A] {
	f := func(items []A) bool {
		return len(items) > 0
	}
	return Check(f, Many0(parser))
}

func Seq[E, S, T, A any](parsers []*Parser[E, S, T, A]) *Parser[E, S, T, []A] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, []A]] {
		vals := []A{}
		state := s
		tokens := xs
		for _, p := range parsers {
			r := p.Parse(tokens, state)
			if r.Success != nil {
				vals = append(vals, r.Success.Value.Result)
				state = r.Success.Value.State
				tokens = r.Success.Value.Rest
			} else if r.Failure != nil {
				return maybeerror.NewFailure[E, *ParseResult[T, S, []A]]()
			} else {
				return maybeerror.NewError[E, *ParseResult[T, S, []A]](r.Error.Value)
			}
		}
		return good[E, S, T, []A](vals, tokens, state)
	}
	return NewParser(f)
}

/*
// TODO intent:
//   Parser e s (m t) (a -> ... -> z) -> Parser e s (m t) a -> ... -> Parser e s (m t) z
// how best to do this?
func AppP(p *Parser, parsers []*Parser) *Parser {
	g := func(fi interface{}) *Parser {
		f := fi.(func(interface{}) interface{})
		return Fmap(func(args interface{}) interface{} { return f(args) }, Seq(parsers))
	}
	return Bind(p, g)
}
*/

func AppP[E, S, T, A, B any](p1 *Parser[E, S, T, func(A) B], p2 *Parser[E, S, T, A]) *Parser[E, S, T, B] {
	return Bind[E, S, T, func(A) B](p1, func(f func(A) B) *Parser[E, S, T, B] {
		return Bind[E, S, T, A](p2, func(x A) *Parser[E, S, T, B] {
			return Pure[E, S, T, B](f(x))
		})
	})
}

func App[E, S, T, A, B any](f func(A) B, p *Parser[E, S, T, A]) *Parser[E, S, T, B] {
	return AppP[E, S, T, A, B](Pure[E, S, T, func(A) B](f), p)
}

func AppP2[E, S, T, A, B, C any](
	p1 *Parser[E, S, T, func(A, B) C],
	p2 *Parser[E, S, T, A],
	p3 *Parser[E, S, T, B]) *Parser[E, S, T, C] {
	return Bind[E, S, T, func(A, B) C](p1, func(f func(A, B) C) *Parser[E, S, T, C] {
		return Bind[E, S, T, A](p2, func(x A) *Parser[E, S, T, C] {
			return Bind[E, S, T, B](p3, func(y B) *Parser[E, S, T, C] {
				return Pure[E, S, T, C](f(x, y))
			})
		})
	})
}

func App2[E, S, T, A, B, C any](f func(A, B) C, p1 *Parser[E, S, T, A], p2 *Parser[E, S, T, B]) *Parser[E, S, T, C] {
	return AppP2[E, S, T, A, B, C](Pure[E, S, T, func(A, B) C](f), p1, p2)
}

func Seq2L[E, S, T, A, B any](p1 *Parser[E, S, T, A], p2 *Parser[E, S, T, B]) *Parser[E, S, T, A] {
	f := func(a A, b B) A {
		return a
	}
	return App2(f, p1, p2)
}

func Seq2R[E, S, T, A, B any](p1 *Parser[E, S, T, A], p2 *Parser[E, S, T, B]) *Parser[E, S, T, B] {
	f := func(a A, b B) B {
		return b
	}
	return App2(f, p1, p2)
}

func Repeat[E, S, T, A any](count int, parser *Parser[E, S, T, A]) *Parser[E, S, T, []A] {
	parsers := make([]*Parser[E, S, T, A], count)
	for i := 0; i < count; i++ {
		parsers[i] = parser
	}
	return Seq(parsers)
}

/*
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
*/
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
