package generics

import "fmt"

// this example is from: https://go.googlesource.com/proposal/+/refs/heads/master/design/43651-type-parameters.md#using-types-that-refer-to-themselves-in-constraints

type Eq[T any] interface {
	Equal(T) bool
}

type Uint uint

func (u Uint) Equal(y Uint) bool {
	return u == y
}

type Comparable[T comparable] struct {
	T T
}

func (c *Comparable[T]) Equal(other *Comparable[T]) bool {
	return c.T == other.T
}

func Index[T Eq[T]](s []T, e T) int {
	for i, v := range s {
		if e.Equal(v) {
			return i
		}
	}
	return -1
}

func EqExample() {
	a := []Uint{1, 2, 3, 4, 5}
	for _, x := range []Uint{0, 2, 4, 6, 8} {
		fmt.Printf("looking for %d: result %d\n", x, Index(a, x))
	}
}
