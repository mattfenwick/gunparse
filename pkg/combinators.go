package pkg

import (
	"github.com/mattfenwick/gunparse/pkg/maybeerror"
	"reflect"
)

type Parser[E, S, T, A any] struct {
	Parse func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]]
}

func NewParser[E, S, T, A any](parse func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]]) *Parser[E, S, T, A] {
	return &Parser[E, S, T, A]{Parse: parse}
}

func (p *Parser[E, S, T, A]) Run(input []T, state S) *maybeerror.MaybeError[E, *ParseResult[T, S, A]] {
	return p.Parse(input, state)
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

func AppP4[E, S, T, A, B, C, D, M any](
	p1 *Parser[E, S, T, func(A, B, C, D) M],
	p2 *Parser[E, S, T, A],
	p3 *Parser[E, S, T, B],
	p4 *Parser[E, S, T, C],
	p5 *Parser[E, S, T, D]) *Parser[E, S, T, M] {
	return Bind[E, S, T, func(A, B, C, D) M](p1, func(f func(A, B, C, D) M) *Parser[E, S, T, M] {
		return Bind[E, S, T, A](p2, func(x A) *Parser[E, S, T, M] {
			return Bind[E, S, T, B](p3, func(y B) *Parser[E, S, T, M] {
				return Bind[E, S, T, C](p4, func(z C) *Parser[E, S, T, M] {
					return Bind[E, S, T, D](p5, func(l D) *Parser[E, S, T, M] {
						return Pure[E, S, T, M](f(x, y, z, l))
					})
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
	return AppP3[E, S, T, A, B, C, D](Pure[E, S, T, func(A, B, C) D](f), p1, p2, p3)
}

func App4[E, S, T, A, B, C, D, M any](
	f func(A, B, C, D) M,
	p1 *Parser[E, S, T, A],
	p2 *Parser[E, S, T, B],
	p3 *Parser[E, S, T, C],
	p4 *Parser[E, S, T, D]) *Parser[E, S, T, M] {
	return AppP4[E, S, T, A, B, C, D, M](Pure[E, S, T, func(A, B, C, D) M](f), p1, p2, p3, p4)
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

// SepBy0 is similar to SepBy1, except that if no instance of `parser` is found,
//   it presents a value of `nil`.  TODO is this a bad idea?  Should there be a Maybe wrapper instead?
func SepBy0[E, S, T, A, B any](parser *Parser[E, S, T, A], separator *Parser[E, S, T, B]) *Parser[E, S, T, *SepByResult[A, B]] {
	return Optional[E, S, T, *SepByResult[A, B]](SepBy1[E, S, T, A, B](parser, separator), nil)
}

type Itemizer[E, S, T any] struct {
	ProcessState func(T, S) S
	Item         *Parser[E, S, T, T]
}

func NewItemizer[E, S, T any](processState func(T, S) S) *Itemizer[E, S, T] {
	it := &Itemizer[E, S, T]{ProcessState: processState}
	it.Item = it.item()
	return it
}

func (i *Itemizer[E, S, T]) item() *Parser[E, S, T, T] {
	g := func(xs []T, s S) *maybeerror.MaybeError[E, *ParseResult[T, S, T]] {
		if len(xs) == 0 {
			return maybeerror.NewFailure[E, *ParseResult[T, S, T]]()
		}
		head := xs[0]
		tail := xs[1:]
		return good[E, S, T, T](head, tail, i.ProcessState(head, s))
	}
	return NewParser(g)
}

func (i *Itemizer[E, S, T]) Satisfy(pred func(T) bool) *Parser[E, S, T, T] {
	return Check(pred, i.Item)
}

func (i *Itemizer[E, S, T]) Literal(x T) *Parser[E, S, T, T] {
	return i.Satisfy(func(y T) bool {
		return reflect.DeepEqual(x, y)
	})
}

func Not1[E, S, T, A any](i *Itemizer[E, S, T], parser *Parser[E, S, T, A]) *Parser[E, S, T, T] {
	return Seq2R(Not0(parser), i.Item)
}

func (i *Itemizer[E, S, T]) MatchString(xs []T) *Parser[E, S, T, []T] {
	return Seq(mapList(i.Literal, xs))
}

func (i *Itemizer[E, S, T]) OneOf(elems []T) *Parser[E, S, T, T] {
	elemSet := map[interface{}]bool{}
	for _, e := range elems {
		elemSet[e] = true
	}
	return i.Satisfy(func(t T) bool {
		return elemSet[t]
	})
}

func BasicItemizer[E, S, T any]() *Itemizer[E, S, T] {
	return NewItemizer[E, S, T](second[T, S])
}

func PositionItemizer[E any]() *Itemizer[E, *Pair[int, int], rune] {
	return NewItemizer[E, *Pair[int, int], rune](func(t rune, s *Pair[int, int]) *Pair[int, int] {
		// TODO verify that runes actually work fine like this
		line, col := s.A, s.B
		if t == '\n' {
			return NewPair(line+1, 1)
		} else {
			return NewPair(line, col+1)
		}
	})
}

func CountItemizer[E, T any]() *Itemizer[E, int, T] {
	return NewItemizer[E, int, T](func(t T, s int) int {
		return s + 1
	})
}
