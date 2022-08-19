package codegen

import (
	"encoding/json"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"
	"unicode"

	"github.com/uhppoted/uhppoted-codegen/model"
)

type Generator struct {
	models    string
	templates string
	out       string
	debug     bool
}

var funcs = template.FuncMap{
	"lowercase": lowercase,
	"uppercase": uppercase,
	"trim":      trim,
	"CamelCase": CamelCase,
	"camelCase": camelCase,
	"kebabCase": kebabCase,
	"snakeCase": snakeCase,
	"lookup": func(path, key, defval string) any {
		return lookup(map[string]any{}, path, key, defval)
	},
}

var uhppote = model.Model{
	Functions: model.Functions,
	Requests:  model.Requests,
	Responses: model.Responses,
}

func New(models string, templates string, out string, debug bool) Generator {
	g := Generator{
		models:    models,
		templates: templates,
		out:       out,
		debug:     debug,
	}

	return g
}

func (g Generator) Generate() error {
	data, err := g.initialise()
	if err != nil {
		return err
	}

	funcs["lookup"] = func(path, key, defval string) any {
		return lookup(data, path, key, defval)
	}

	fsys := os.DirFS(g.templates)

	if err := os.MkdirAll(g.out, 0777); err != nil {
		return err
	}

	f := func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		} else if d.IsDir() {
			return nil
		} else if strings.HasPrefix(path, ".templates") {
			return nil
		}

		return g.generate(fsys, path, data, funcs)
	}

	if err := fs.WalkDir(fsys, ".", f); err != nil {
		return err
	}

	return nil
}

func (g Generator) initialise() (map[string]any, error) {
	fsys := os.DirFS(g.models)

	data := map[string]any{
		"model": uhppote,
	}

	read := func(path string) error {
		m := map[string]any{}

		if bytes, err := fs.ReadFile(fsys, path); err != nil {
			return err
		} else if err := json.Unmarshal(bytes, &m); err != nil {
			return err
		} else {
			for k, v := range m {
				data[k] = v
			}

			return nil
		}
	}

	f := func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		} else if d.IsDir() {
			return nil
		}

		return read(path)
	}

	if err := fs.WalkDir(fsys, ".", f); err != nil {
		return nil, err
	}

	return data, nil
}

func (g Generator) generate(fsys fs.FS, src string, data any, functions template.FuncMap) error {
	bytes, err := fs.ReadFile(fsys, src)
	if err != nil {
		return err
	}

	t, err := template.New("codegen").Funcs(functions).Parse(string(bytes))
	if err != nil {
		return err
	}

	// ... attach common templates

	if _, err := fs.Stat(fsys, ".templates"); err == nil {
		if list, err := fs.Glob(fsys, ".templates/**"); err == nil && len(list) > 0 {
			if t, err = t.ParseFS(fsys, ".templates/**"); err != nil {
				return err
			}
		} else if err != nil {
			return err
		}
	} else if !os.IsNotExist(err) {
		return err
	}

	// ... generate to tempfile

	f, err := os.CreateTemp("", "uhppote-*.go")
	if err != nil {
		return err
	} else {
		defer os.Remove(f.Name())
	}

	t.Execute(f, data)

	// ... copy to destination file
	dest := filepath.Join(g.out, src)

	if err := os.MkdirAll(filepath.Dir(dest), 0777); err != nil {
		return err
	}

	if err := os.Rename(f.Name(), dest); err != nil {
		return err
	}

	return nil
}

func lowercase(s string) string {
	return strings.ToLower(s)
}

func uppercase(s string) string {
	return strings.ToUpper(s)
}

func trim(s string) string {
	return strings.TrimSpace(s)
}

func CamelCase(s string) string {
	tokens := regexp.MustCompile(`\s+`).Split(s, -1)

	for i, token := range tokens {
		tokens[i] = capitalize(token)
	}

	return strings.Join(tokens, "")
}

func camelCase(s string) string {
	tokens := regexp.MustCompile(`\s+`).Split(s, -1)

	for i, token := range tokens[1:] {
		tokens[i+1] = capitalize(token)
	}

	return strings.Join(tokens, "")
}

func kebabCase(s string) string {
	tokens := regexp.MustCompile(`\s+`).Split(s, -1)

	for i, token := range tokens[1:] {
		tokens[i+1] = strings.ToLower(token)
	}

	return strings.Join(tokens, "-")
}

func snakeCase(s string) string {
	tokens := regexp.MustCompile(`\s+`).Split(s, -1)

	for i, token := range tokens {
		tokens[i] = strings.ToLower(token)
	}

	return strings.Join(tokens, "_")
}

func capitalize(s string) string {
	runes := []rune(s)

	if len(runes) > 0 {
		runes[0] = unicode.ToUpper(runes[0])
	}

	return string(runes)
}

func lookup(data map[string]any, path, key, defval string) any {
	p := strings.Split(path, ".")
	table := data

	for _, k := range p {
		if v, ok := table[k]; !ok {
			return defval
		} else if t, ok := v.(map[string]any); !ok {
			return defval
		} else {
			table = t
		}
	}

	if v, ok := table[key]; ok {
		return v
	}

	return defval
}
