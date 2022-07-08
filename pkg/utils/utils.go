package utils

import "github.com/pkg/errors"

func DoOrDie(err error) {
	if err != nil {
		panic(errors.Wrapf(err, "dying"))
	}
}
