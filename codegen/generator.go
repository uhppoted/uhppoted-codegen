package codegen

import (
	"io/fs"
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
	data := map[string]any{}
	fsys := os.DirFS(g.templates)

	if err := os.MkdirAll(g.out, 0777); err != nil {
		return err
	}

	f := func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		} else if d.IsDir() {
			return nil
		}

		return g.generate(fsys, path, data)
	}

	if err := fs.WalkDir(fsys, ".", f); err != nil {
		return err
	}

	return nil
}

func (g Generator) generate(fsys fs.FS, src string, data map[string]any) error {
	dest := filepath.Join(g.out, src)

	if err := os.MkdirAll(filepath.Dir(dest), 0777); err != nil {
		return err
	}

	file, err := os.Create(dest)
	if err != nil {
		return err
	}

	defer file.Close()

	t, err := template.ParseFS(fsys, src)
	if err != nil {
		return err
	}

	t.Execute(file, data)

	return nil
}
