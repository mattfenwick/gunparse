package generics

// this example is from: https://go.googlesource.com/proposal/+/refs/heads/master/design/43651-type-parameters.md#using-types-that-refer-to-themselves-in-constraints

type Eq[T any] interface {
	Equal(T) bool
}

func (a Int) Equal(b Int) bool {
	return a == b
}

func (a Uint) Equal(b Uint) bool {
	return a == b
}

func (a Bool) Equal(b Bool) bool {
	return a == b
}

func (a String) Equal(b String) bool {
	return a == b
}

func (xs SliceEq[A]) Equal(ys SliceEq[A]) bool {
	if len(xs) != len(ys) {
		return false
	}
	for i := range xs {
		if !xs[i].Equal(ys[i]) {
			return false
		}
	}
	return true
}

// TODO any way to use this?
//func EqualComparable[T comparable](a T, b T) bool {
//	return a == b
//}
