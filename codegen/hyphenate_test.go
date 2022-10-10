package codegen

import (
	"testing"
)

func TestHyphenate(t *testing.T) {
	tests := []struct {
		s        string
		expected string
	}{
		{"qwerty", "qwerty"},
		{"qwerty uiop", "qwerty-uiop"},
		{"qwerty-uiop", "qwerty-uiop"},
		{"qwerty_uiop", "qwerty-uiop"},
		{"qwerty  uiop", "qwerty-uiop"},
		{"qwerty--uiop", "qwerty-uiop"},
		{"qwerty__uiop", "qwerty-uiop"},
		{" qwerty uiop", "qwerty-uiop"},
		{"qwerty uiop ", "qwerty-uiop"},
	}

	for _, test := range tests {
		if s := hyphenate(test.s); s != test.expected {
			t.Errorf("%v: incorrectly hyphenated - expected:%v, got:%v", test.s, test.expected, s)
		}
	}
}
