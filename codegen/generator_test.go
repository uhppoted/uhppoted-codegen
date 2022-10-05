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
