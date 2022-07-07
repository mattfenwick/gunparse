package main

import (
	"encoding/json"
	"fmt"
	"github.com/mattfenwick/gunparse/pkg"
	"github.com/mattfenwick/gunparse/pkg/example"
	"github.com/mattfenwick/gunparse/pkg/utils"
)

func main() {
	strings := []string{
		"true",
		"trueqrs",
		"true qrs",
		"false123",
		"false 123",
		"abc",
		"",
	}
	for _, s := range strings {
		result := example.KeywordParser.Parse(example.StringToRunes(s), pkg.NewPair(1, 1))
		dumped, err := json.MarshalIndent(result, "", "  ")
		utils.DoOrDie(err)
		fmt.Printf("%s\n\n", dumped)
	}
}
