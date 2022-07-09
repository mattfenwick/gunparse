package generics

func listOfTypesTodo(c complex64) {
	//type bool bool
	//type uint8 uint8
	//type uint16 uint16
	//type uint32 uint32
	//type uint64 uint64
	//type int8 int8
	//type int16 int16
	//type int32 int32
	//type int64 int64
	//type float32 float32
	//type float64 float64
	//type complex64 complex64
	//type complex128 complex128
	//
	//type string string
	//type int int
	//type uint uint
	//type uintptr uintptr
	//type byte = uint8
	//type rune = int32
}

type Bool bool

type Uint uint
type Uint8 uint8
type Uint16 uint16
type Uint32 uint32
type Uint64 uint64

type Int int
type Int8 int8
type Int16 int16
type Int32 int32
type Int64 int64

type Float32 float32
type Float64 float64

type Complex64 complex128
type Complex128 complex128

type String string

type SliceEq[A Eq[A]] []A
type SliceOrd[A Ord[A]] []A

// EqOrComparable allows us to avoid getting "invalid map key type A (missing comparable constraint)"
//   errors, if we just used Eq[A] without this additional interface
type EqOrComparable[A Eq[A]] interface {
	Eq[A]
	comparable
}

type MapEq[A EqOrComparable[A], B Eq[B]] map[A]B
