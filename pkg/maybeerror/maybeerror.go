package maybeerror

import "fmt"

// Status ...
type Status int

// .....
const (
	StatusSuccess Status = iota
	StatusFailure Status = iota
	StatusError   Status = iota
)

// String .....
func (status Status) String() string {
	switch status {
	case StatusSuccess:
		return "StatusSuccess"
	case StatusFailure:
		return "StatusFailure"
	case StatusError:
		return "StatusError"
	}
	panic(fmt.Errorf("invalid Status value: %d", status))
}

// MarshalJSON .....
func (status Status) MarshalJSON() ([]byte, error) {
	jsonString := fmt.Sprintf(`"%s"`, status.String())
	return []byte(jsonString), nil
}

// MarshalText .....
func (status Status) MarshalText() (text []byte, err error) {
	return []byte(status.String()), nil
}

// MaybeError ...
type MaybeError struct {
	Status Status
	Value  interface{}
}

// NewMaybeError ...
func NewMaybeError(status Status, value interface{}) *MaybeError {
	return &MaybeError{Status: status, Value: value}
}

// Pure ...
func Pure(value interface{}) *MaybeError {
	return NewMaybeError(StatusSuccess, value)
}

// Error ...
func Error(value interface{}) *MaybeError {
	return NewMaybeError(StatusError, value)
}

// Fmap ...
func (me *MaybeError) Fmap(f func(interface{}) interface{}) *MaybeError {
	if me.Status == StatusSuccess {
		return Pure(f(me.Value))
	}
	return me
}

// App ...
func App(f func([]interface{}) interface{}, vals ...*MaybeError) *MaybeError {
	args := make([]interface{}, len(vals))
	for i, v := range vals {
		if v.Status == StatusSuccess {
			args[i] = v.Value
		} else {
			return v
		}
	}
	return Pure(f(args))
}

// Bind ...
func (me *MaybeError) Bind(f func(interface{}) *MaybeError) *MaybeError {
	if me.Status == StatusSuccess {
		return f(me.Value)
	}
	return me
}

// MapError ...
func (me *MaybeError) MapError(f func(interface{}) interface{}) *MaybeError {
	if me.Status == StatusError {
		return Error(f(me.Value))
	}
	return me
}

// Plus ...
func (me *MaybeError) Plus(other *MaybeError) *MaybeError {
	if me.Status == StatusFailure {
		return other
	}
	return me
}

// Zero ...
var Zero = NewMaybeError(StatusFailure, nil)
