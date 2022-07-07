package pkg

import "github.com/mattfenwick/gunparse/pkg/maybeerror"

// Unit represents the Haskell value `()`
type Unit struct{}

var UnitC = &Unit{}

type Pair[A, B any] struct {
	A A
	B B
}

func NewPair[A, B any](a A, b B) *Pair[A, B] {
	return &Pair[A, B]{A: a, B: b}
}

type SepByResult[A, B any] struct {
	First A
	Pairs []*Pair[B, A]
}

func NewSepByResult[A, B any](first A, pairs []*Pair[B, A]) *SepByResult[A, B] {
	return &SepByResult[A, B]{First: first, Pairs: pairs}
}

func (s *SepByResult[A, B]) Values() []A {
	if s == nil {
		return nil
	}
	vals := []A{s.First}
	for _, p := range s.Pairs {
		vals = append(vals, p.B)
	}
	return vals
}

func (s *SepByResult[A, B]) Separators() []B {
	seps := []B{}
	for _, p := range s.Pairs {
		seps = append(seps, p.A)
	}
	return seps
}

type Maybe[A any] struct {
	Success *maybeerror.Success[A]
	Failure *maybeerror.Failure
}
