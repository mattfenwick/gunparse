package types

import (
	"fmt"
	"strings"
)

type Field struct {
	Name     string
	TypeName string
}

type Type struct {
	Name   string
	Fields []*Field
}

const (
	structDefTemplate = `type %s struct {
%s
}`

	constructorTemplate = `func New%s(%s) *%s{
    return &%s{
%s
    }
}`
)

var (
	Model = []*Type{
		{"Decimal", []*Field{
			{"Dot", "rune"},
			{"Digits", "[]rune"}}},
		{"Exponent", []*Field{
			{"Letter", "rune"},
			{"Sign", "rune"},
			{"Power", "[]rune"}}},
		{"Number", []*Field{
			{"Sign", "rune"},
			{"Integer", "[]rune"},
			{"Decimal", "*Decimal"},
			{"Exponent", "*Exponent"}}},
		{"Character", []*Field{
			{"Value", "rune"}}},
		{"Escape", []*Field{
			{"Open", "rune"},
			{"Value", "rune"}}},
		{"UnicodeEscape", []*Field{
			{"Open", "[]rune"},
			{"Value", "[]rune"}}},
		{"StringChar", []*Field{
			{"Char", "*Character"},
			{"Escape", "*Escape"},
			{"UnicodeEscape", "*UnicodeEscape"}}},
		{"String", []*Field{
			{"Open", "rune"},
			{"Value", "[]*StringChar"},
			{"Close", "rune"}}},
		{"Keyword", []*Field{
			{"Value", "[]rune"}}},
		{"KeyValPair", []*Field{
			{"Key", "*String"},
			{"Colon", "rune"},
			{"Value", "*JsonValue"}}},
		{"Array", []*Field{
			{"Open", "rune"},
			{"Body", "[]*JsonValue"},
			{"Close", "rune"}}},
		{"Object", []*Field{
			{"Open", "rune"},
			{"Body", "[]*KeyValPair"},
			{"Close", "rune"}}},
		{"JsonValue", []*Field{
			{"String", "*String"},
			{"Number", "*Number"},
			{"Keyword", "*Keyword"},
			{"Object", "*Object"},
			{"Array", "*Array"}}},
	}
)

func ModelToText() {
	fmt.Printf("package example\n\n")

	for _, t := range Model {
		var fieldDefinitions []string
		var fieldParams []string
		var fieldInitializers []string
		for _, f := range t.Fields {
			fieldDefinitions = append(fieldDefinitions, fmt.Sprintf("    %s %s", f.Name, f.TypeName))
			fieldParams = append(fieldParams, fmt.Sprintf("%s %s", strings.ToLower(f.Name), f.TypeName))
			fieldInitializers = append(fieldInitializers, fmt.Sprintf("        %s: %s,", f.Name, strings.ToLower(f.Name)))
		}

		// print type definition
		fmt.Printf(structDefTemplate,
			t.Name,
			strings.Join(fieldDefinitions, "\n"))

		fmt.Printf("\n\n")

		// print constructor
		fmt.Printf(constructorTemplate,
			t.Name,
			strings.Join(fieldParams, ", "),
			t.Name,
			t.Name,
			strings.Join(fieldInitializers, "\n"))

		fmt.Printf("\n\n")
	}

	fmt.Println()
}
