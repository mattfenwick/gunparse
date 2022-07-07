package pkg

//type ErrorStack[E, S any] struct {
//	Errors []*Pair[E, S]
//}

func NewErrorStack[E, S any](e E, s S) []*Pair[E, S] {
	return []*Pair[E, S]{{A: e, B: s}}
}

func Cut[E, S, T, A any](e E, p *Parser[[]*Pair[E, S], S, T, A]) *Parser[[]*Pair[E, S], S, T, A] {
	f := func(state S) *Parser[[]*Pair[E, S], S, T, A] {
		return Commit(NewErrorStack(e, state), p)
	}
	return Bind[[]*Pair[E, S], S, T, S, A](GetState[[]*Pair[E, S], S, T](), f)
}

func AddErrorState[E, S, T, A any](e E, p *Parser[[]*Pair[E, S], S, T, A]) *Parser[[]*Pair[E, S], S, T, A] {
	f := func(state S) *Parser[[]*Pair[E, S], S, T, A] {
		return AddError(NewPair(e, state), p)
	}
	return Bind[[]*Pair[E, S], S, T, S, A](GetState[[]*Pair[E, S], S, T](), f)
}

/*
def _forbid_duplicates(arr):
    keys = set()
    for a in arr:
        if a in keys:
            raise Exception("duplicate name -- {}".format(a))
        keys.add(a)

def _forbid_keys(forbidden, keys):
    key_set = set(keys)
    for key in forbidden:
        if key in key_set:
            raise Exception('cst node: forbidden key: {}'.format(key))

# wish I could put `pairs` in a kwargs dictionary, but then the order would be lost
def node(name, *pairs):
    """
    1. runs parsers in sequence
    2. collects results into a dictionary
    3. grabs state at which parsers started
    4. adds an error frame
    """
    names = [x for (x, _) in pairs]
    _forbid_duplicates(names)
    _forbid_keys(['_name', '_start', '_end'], names)
    def action(start, results, end):
        out = dict(results)
        out['_start'] = start
        out['_name'] = name
        out['_end'] = end
        return out
    def closure_workaround(a):
        '''captures value of a'''
        return lambda b: (a, b)
    child_parsers = seq([fmap(closure_workaround(parser_name), parser) for (parser_name, parser) in pairs])
    return addErrorState(name, app(action, getState, child_parsers, getState))
*/
