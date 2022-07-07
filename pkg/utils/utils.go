package utils

func DoOrDie(err error) {
	if err != nil {
		panic(err)
	}
}
