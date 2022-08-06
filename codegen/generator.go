package codegen

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"
	"unicode"
)

type Generator struct {
	templates string
	out       string
	debug     bool
}

var funcs = template.FuncMap{
	"trim":      trim,
	"CamelCase": CamelCase,
	"camelCase": camelCase,
	"kebabCase": kebabCase,
	"lookup":    lookup,
	"stash":     stash,
}

var kv = map[string]string{}

var data = struct {
	Functions []function
	Requests  []request
	Responses []response
}{
	Functions: functions,
	Requests:  requests,
	Responses: responses,
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

	f := func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		} else if d.IsDir() {
			return nil
		} else if strings.HasPrefix(path, "_") {
			return nil
		}

		return g.generate(fsys, path, data, funcs)
	}

	if err := fs.WalkDir(fsys, ".", f); err != nil {
		return err
	}

	return nil
}

func (g Generator) generate(fsys fs.FS, src string, data any, functions template.FuncMap) error {
	bytes, err := fs.ReadFile(fsys, src)
	if err != nil {
		return err
	}

	dest := filepath.Join(g.out, src)

	if err := os.MkdirAll(filepath.Dir(dest), 0777); err != nil {
		return err
	}

	file, err := os.Create(dest)
	if err != nil {
		return err
	}

	defer file.Close()

	t, err := template.New("codegen").Funcs(functions).Parse(string(bytes))
	if err != nil {
		return err
	}

	t.Execute(file, data)

	return nil
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

func capitalize(s string) string {
	runes := []rune(s)

	if len(runes) > 0 {
		runes[0] = unicode.ToUpper(runes[0])
	}

	return string(runes)
}

func lookup(s string) string {
	if v, ok := kv[s]; ok {
		return v
	}

	return ""
}

func stash(key string, value string) string {
	kv[key] = value
	return ""
}
