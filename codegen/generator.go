package codegen

import (
	"fmt"
)

type Generator struct {
	templates string
	out       string
	debug     bool
}

func New(templates string, out string, debug bool) Generator {
	g := Generator{
		templates: templates,
		out:       out,
		debug:     debug,
	}

	return g
}

func (g Generator) Generate() error {
	return fmt.Errorf("NOT IMPLEMENTED")
}
