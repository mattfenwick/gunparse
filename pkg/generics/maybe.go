package generics

type Maybe[A any] struct {
	Value *A
}

func Just[A any](a A) *Maybe[A] {
	return &Maybe[A]{Value: &a}
}

func Nothing[A any]() *Maybe[A] {
	return &Maybe[A]{Value: nil}
}

func MapMaybe[A, B any](f F1[A, B], m Maybe[A]) *Maybe[B] {
	if m.Value == nil {
		return Nothing[B]()
	}
	return Just(f(*m.Value))
}

func BindMaybe[A, B any](m Maybe[A], f F1[A, *Maybe[B]]) *Maybe[B] {
	if m.Value == nil {
		return Nothing[B]()
	}
	return f(*m.Value)
}
