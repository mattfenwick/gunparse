package generics

type List[A any] struct {
	Head A
	Tail *List[A] // TODO use maybe
}
