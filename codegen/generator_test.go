package codegen

import (
	"testing"
)

func TestConstantFunc(t *testing.T) {
	tests := []struct {
		s        string
		expected string
	}{
		{"qwerty", "QWERTY"},
		{"qwerty uiop", "QWERTY_UIOP"},
		{"qwerty   uiop", "QWERTY_UIOP"},
		{" qwerty uiop", "QWERTY_UIOP"},
		{"qwerty uiop ", "QWERTY_UIOP"},
		{"qwerty-uiop", "QWERTY_UIOP"},
		{"-qwerty uiop", "_QWERTY_UIOP"},
		{"qwerty uiop-", "QWERTY_UIOP_"},
	}

	for _, v := range tests {
		if s := constant(v.s); s != v.expected {
			t.Errorf("%q: Invalid 'constant' - expected:%q, got:%q", v.s, v.expected, s)
		}
	}
}

func TestIgnoreGlob(t *testing.T) {
	tests := []struct {
		pattern  string
		path     string
		expected bool
	}{
		{".DS_Store", "src/commands.lua", false},
		{".DS_Store", ".DS_Store", true},
		{"**/*.swp", "src/.encode.lua.swp", true},
	}

	for _, v := range tests {
		if i := ignore(v.pattern, v.path); i != v.expected {
			t.Errorf("%q %q: incorrect 'ignore' result - expected:%v, got:%v", v.pattern, v.path, v.expected, i)
		}
	}
}
