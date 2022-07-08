package main

import (
	"encoding/json"
	"fmt"
	"github.com/mattfenwick/gunparse/pkg/utils"
)

func main() {
	a := &StuffA{Const: "mnop"}
	b := &StuffB{}
	c := &StuffC[int]{}

	fmt.Println(DoStuff(a, a))
	fmt.Println(DoStuff(b, b))
	//fmt.Println(DoStuff(a, b))
	//fmt.Println(DoStuff(b, a))

	fmt.Println(Example(a, a))
	fmt.Println(Example(c, c))

	g := &Generic[string, Getter[string], Setter[string]]{
		A: "abc",
	}
	g.B = &GetterImpl[string]{A: "qrs"} // g
	g.C = g
	fmt.Printf("%+v\n%s\n", g.Get(), g.GenericFunc())
}

type Stuff interface {
	DoStuff() string
}

type StuffA struct {
	Const string
}

func (s *StuffA) DoStuff() string {
	return s.Const
}

type StuffB struct{}

func (s *StuffB) DoStuff() string {
	return "abc"
}

type StuffC[X any] struct {
	FieldX X
}

func DoStuff[A Stuff](a A, b A) string {
	return fmt.Sprintf("%s: %s", a.DoStuff(), b.DoStuff())
}

func Example[A any](a A, b A) string {
	dumped, err := json.MarshalIndent([]interface{}{a, b}, "", "  ")
	utils.DoOrDie(err)
	return string(dumped)
}

type IntOrString interface {
	int | string
}

type Setter[A any] interface {
	Set(A)
}

type Getter[A any] interface {
	Get() A
}

type GetterImpl[A any] struct{ A A }

func (g *GetterImpl[A]) Get() A {
	return g.A
}

type Generic[A IntOrString, B Getter[A], C Setter[A]] struct {
	A A
	B B
	C C
}

func (g *Generic[A, B, C]) GenericFunc() string {
	one := g.B.Get()
	two := g.Get()

	g.C.Set(two)
	g.Set(one)

	return fmt.Sprintf("%+v\n%+v\n%+v\n%+v\n\n", one, two, g.B.Get(), g.Get())
}

func (g *Generic[A, B, C]) Get() A {
	return g.A
}

func (g *Generic[A, B, C]) Set(a A) {
	g.A = a
}
