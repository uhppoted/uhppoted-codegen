package codegen

import (
	//	"fmt"
	"os"
	"path/filepath"
	"text/template"
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
	fsys := os.DirFS(g.templates)

	if err := os.MkdirAll(g.out, 0777); err != nil {
		return err
	}

	templates, err := template.ParseFS(fsys, "*")
	if err != nil {
		return err
	}

	list := templates.Templates()
	for _, t := range list {
		g.generate(t)
	}

	return nil
}

func (g Generator) generate(t *template.Template) error {
	data := map[string]string{}
	path := filepath.Join(g.out, t.Name())

	file, err := os.Create(path)
	if err != nil {
		return err
	}

	defer file.Close()

	t.Execute(file, data)

	return nil
}
