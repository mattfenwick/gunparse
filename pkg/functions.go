package pkg

func compose(f func(interface{}) interface{}, g func(interface{}) interface{}) func(interface{}) interface{} {
	return func(x interface{}) interface{} {
		return f(g(x))
	}
}

func first(x interface{}, y interface{}) interface{} {
	return x
}

func second(x interface{}, y interface{}) interface{} {
	return y
}

func pair(x interface{}, y interface{}) []interface{} {
	return []interface{}{x, y}
}

func constF(x interface{}) func(interface{}) interface{} {
	return func(y interface{}) interface{} {
		return x
	}
}

func id(x interface{}) interface{} {
	return x
}

//def cons(first, rest):
//    return [first] + rest

func replicate(count int, item interface{}) []interface{} {
	out := make([]interface{}, count)
	for i := 0; i < count; i++ {
		out[i] = item
	}
	return out
}

func flipApply(x interface{}, f func(interface{}) interface{}) interface{} {
	return f(x)
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
