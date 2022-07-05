package pkg

import (
	"github.com/mattfenwick/gunparse/pkg/maybeerror"
)

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

func AppP[E, S, T, A, B any](p1 *Parser[E, S, T, func(A) B], p2 *Parser[E, S, T, A]) *Parser[E, S, T, B] {
	return Bind[E, S, T, func(A) B](p1, func(f func(A) B) *Parser[E, S, T, B] {
		return Bind[E, S, T, A](p2, func(x A) *Parser[E, S, T, B] {
			return Pure[E, S, T, B](f(x))
		})
	})
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

func AppP3[E, S, T, A, B, C, D any](
	p1 *Parser[E, S, T, func(A, B, C) D],
	p2 *Parser[E, S, T, A],
	p3 *Parser[E, S, T, B],
	p4 *Parser[E, S, T, C]) *Parser[E, S, T, D] {
	return Bind[E, S, T, func(A, B, C) D](p1, func(f func(A, B, C) D) *Parser[E, S, T, D] {
		return Bind[E, S, T, A](p2, func(x A) *Parser[E, S, T, D] {
			return Bind[E, S, T, B](p3, func(y B) *Parser[E, S, T, D] {
				return Bind[E, S, T, C](p4, func(z C) *Parser[E, S, T, D] {
					return Pure[E, S, T, D](f(x, y, z))
				})
			})
		})
	})
}

func App[E, S, T, A, B any](f func(A) B, p *Parser[E, S, T, A]) *Parser[E, S, T, B] {
	return AppP[E, S, T, A, B](Pure[E, S, T, func(A) B](f), p)
}

func App2[E, S, T, A, B, C any](
	f func(A, B) C,
	p1 *Parser[E, S, T, A],
	p2 *Parser[E, S, T, B]) *Parser[E, S, T, C] {
	return AppP2[E, S, T, A, B, C](Pure[E, S, T, func(A, B) C](f), p1, p2)
}

func App3[E, S, T, A, B, C, D any](
	f func(A, B, C) D,
	p1 *Parser[E, S, T, A],
	p2 *Parser[E, S, T, B],
	p3 *Parser[E, S, T, C]) *Parser[E, S, T, D] {
	return AppP3[E, S, T, A, B, C](Pure[E, S, T, func(A, B, C) D](f), p1, p2, p3)
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

func Lookahead[E, S, T, A any](parser *Parser[E, S, T, A]) *Parser[E, S, T, A] {
	return Bind(Get[E, S, T](), func(xs []T) *Parser[E, S, T, A] {
		return Bind(GetState[E, S, T](), func(s S) *Parser[E, S, T, A] {
			return Bind(parser, func(a A) *Parser[E, S, T, A] {
				return App2(
					func(_ []T, _ S) A { return a },
					Put[E, S, T](xs),
					PutState[E, S, T](s))
			})
		})
	})
}

func Not0[E, S, T, A any](parser *Parser[E, S, T, A]) *Parser[E, S, T, *Unit] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, *Unit]] {
		r := parser.Parse(xs, s)
		if r.Error != nil {
			return maybeerror.NewError[E, *ParseResult[T, S, *Unit]](r.Error.Value)
		} else if r.Failure != nil {
			return good[E, S, T, *Unit](UnitC, xs, s)
		}
		return maybeerror.NewFailure[E, *ParseResult[T, S, *Unit]]()
	}
	return NewParser(f)
}

func Alt[E, S, T, A any](parsers []*Parser[E, S, T, A]) *Parser[E, S, T, A] {
	f := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
		for _, p := range parsers {
			r := p.Parse(xs, s)
			if r.Success != nil {
				return r
			} else if r.Error != nil {
				return r
			}
		}
		return maybeerror.NewFailure[E, *ParseResult[T, S, A]]()
	}
	return NewParser(f)
}

func Optional[E, S, T, A any](parser *Parser[E, S, T, A], defaultValue A) *Parser[E, S, T, A] {
	return Alt[E, S, T, A]([]*Parser[E, S, T, A]{parser, Pure[E, S, T, A](defaultValue)})
}

func CatchError[E, S, T, A any](parser *Parser[E, S, T, A], f func(E) *Parser[E, S, T, A]) *Parser[E, S, T, A] {
	g := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
		v := parser.Parse(xs, s)
		if v.Error != nil {
			return f(v.Error.Value).Parse(xs, s)
		}
		return v
	}
	return NewParser(g)
}

func MapError[E, S, T, A any](f func(E) E, parser *Parser[E, S, T, A]) *Parser[E, S, T, A] {
	g := func(e E) *Parser[E, S, T, A] {
		return NewError[E, S, T, A](f(e))
	}
	return CatchError(parser, g)
}

func Commit[E, S, T, A any](e E, parser *Parser[E, S, T, A]) *Parser[E, S, T, A] {
	return Alt([]*Parser[E, S, T, A]{parser, NewError[E, S, T, A](e)})
}

func AddError[E, S, T, A any](e E, parser *Parser[[]E, S, T, A]) *Parser[[]E, S, T, A] {
	f := func(es []E) []E {
		return append([]E{e}, es...)
	}
	return MapError(f, parser)
}

func SepBy1[E, S, T, A, B any](parser *Parser[E, S, T, A], separator *Parser[E, S, T, B]) *Parser[E, S, T, *SepByResult[A, B]] {
	return App2(
		NewSepByResult[A, B],
		parser,
		Many0(App2(NewPair[B, A], separator, parser)))
}

// TODO
//func SepBy0[E, S, T, A, B any](parser *Parser[E, S, T, A], separator *Parser[E, S, T, B]) *Parser[E, S, T, *Maybe[*SepByResult[A, B]]] {
//	return Optional[E, S, T, *Maybe[*SepByResult[A, B]]](
//		SepBy1(parser, separator),
//		maybeerror.NewFailure[*Unit, *Maybe[*SepByResult[A, B]]]())
//}

/*
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
