package pkg

func compose[A, B, C any](f func(B) C, g func(A) B) func(A) C {
	return func(x A) C {
		return f(g(x))
	}
}

func first[A, B any](x A, y B) A {
	return x
}

func second[A, B any](x A, y B) B {
	return y
}

//func pair[A, B any](x A, y B) {
//	return []interface{}{x, y}
//}

func constF[A, B any](x A) func(B) A {
	return func(y B) A {
		return x
	}
}

func id[A any](x A) A {
	return x
}

//def cons(first, rest):
//    return [first] + rest

func replicate[A any](count int, item A) []A {
	out := make([]A, count)
	for i := 0; i < count; i++ {
		out[i] = item
	}
	return out
}

func flipApply[A, B any](x A, f func(A) B) B {
	return f(x)
}

func mapList[A, B any](f func(A) B, xs []A) []B {
	out := make([]B, len(xs))
	for i, x := range xs {
		out[i] = f(x)
	}
	return out
}

/*
TODO

def updatePosition(char, position):
    """
    only treats `\n` as newline
    """
    line, col = position
    return (line + 1, 1) if (char == '\n') else (line, col + 1)

def applyAll(x, fs):
    return functools.reduce(lambda y, g: g(y), fs, x)

def reverseApplyAll(fs, x):
    return applyAll(x, fs[::-1])
*/
