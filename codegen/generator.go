package codegen

import (
	"bufio"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"text/template"
	"unicode"

	lib "github.com/uhppoted/uhppoted-lib/os"
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
	"pad":       pad,
	"hyphenate": hyphenate,
	"CamelCase": CamelCase,
	"camelCase": camelCase,
	"kebabCase": kebabCase,
	"snakeCase": snakeCase,
	"byte2hex":  byte2hex,

	"add":      add,
	"count":    count,
	"subslice": subslice,
	"last":     last,

	"constant": constant,
	"dump":     dump,
	"lookup": func(path, key, defval string) any {
		return lookup(map[string]any{}, path, key, defval)
	},
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
	ignorable, err := g.ignoreable()
	if err != nil {
		return err
	}

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
		} else {
			for k := range ignorable {
				if k == path {
					return nil
				}

				if matched, err := filepath.Match(k, path); err == nil && matched {
					return nil
				}
			}
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

	data := map[string]any{}

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

func (g Generator) ignoreable() (map[string]struct{}, error) {
	file := filepath.Join(g.templates, ".ignore")
	ignore := map[string]struct{}{
		".ignore": struct{}{},
	}

	if f, err := os.Open(file); err != nil && os.IsNotExist(err) {
		return ignore, nil
	} else if err != nil {
		return ignore, err
	} else {
		defer f.Close()

		scanner := bufio.NewScanner(f)
		for scanner.Scan() {
			ignore[scanner.Text()] = struct{}{}
		}

		return ignore, nil
	}
}

func (g Generator) generate(fsys fs.FS, src string, data any, functions template.FuncMap) error {
	bytes, err := fs.ReadFile(fsys, src)
	if err != nil {
		return err
	}

	t, err := template.New("codegen").Funcs(functions).Parse(string(bytes))
	if err != nil {
		return fmt.Errorf("error parsing %v (%v)", src, err)
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
	f, err := os.CreateTemp("", "uhppote-*")
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

	if err := lib.Rename(f.Name(), dest); err != nil {
		return err
	}

	return nil
}

func ignore(pattern string, path string) bool {
	if path == pattern {
		return true
	}

	if matched, err := filepath.Match(pattern, path); err == nil && matched {
		return true
	}

	return false
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
	tokens := regexp.MustCompile(`[ -]+`).Split(s, -1)

	for i, token := range tokens {
		tokens[i] = capitalize(token)
	}

	return strings.Join(tokens, "")
}

func camelCase(s string) string {
	tokens := regexp.MustCompile(`[ -]+`).Split(s, -1)

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

func byte2hex(v any) string {
	switch b := v.(type) {
	case uint8:
		return fmt.Sprintf("0x%02x", b)
	case float64:
		return fmt.Sprintf("0x%02x", uint8(b))
	}

	return "??"
}

func constant(s string) string {
	tokens := regexp.MustCompile(`[\- ]+`).Split(uppercase(strings.TrimSpace(s)), -1)

	return strings.Join(tokens, "_")
}

func pad(width int, v any) string {
	s := fmt.Sprintf("%v", v)
	if width < len([]rune(s)) {
		return s
	}

	return s + strings.Repeat(" ", width-len([]rune(s)))
}

func add(u, v any) any {
	if iu, ok := u.(int); ok {
		if iv, ok := v.(int); ok {
			return iu + iv
		}
	}

	return nil
}

func count(u any) int {
	N := 0
	if v, ok := u.([]any); ok {
		for _, w := range v {
			if f, ok := w.(map[string]any); ok {
				if ftype, ok := f["type"]; ok && ftype != "magic" {
					N++
				}
			}
		}
	}

	return N
}

func subslice(v any) any {
	if slice, ok := v.([]any); ok && len(slice) > 0 {
		ix := len(slice) - 1
		return slice[0:ix]
	}

	return nil
}

func last(v any) any {
	if slice, ok := v.([]any); ok && len(slice) > 0 {
		ix := len(slice) - 1
		return slice[ix:]
	}

	return nil
}

func dump(v any, prefix string) string {
	bytes, err := base64.StdEncoding.DecodeString(v.(string))
	if err != nil {
		return ""
	}

	lines := []string{}
	ix := 0
	for i := 0; i < 4; i++ {
		line := []string{}
		for j := 0; j < 16; j++ {
			line = append(line, fmt.Sprintf("0x%02x", bytes[ix]))
			ix = ix + 1
		}

		lines = append(lines, prefix+strings.Join(line, ", "))
	}

	return strings.Join(lines, ",\n")
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
