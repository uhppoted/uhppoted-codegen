package codegen

import (
	"regexp"
	"strings"
)

func hyphenate(s string) string {
	tokens := regexp.MustCompile(`[- _]+`).Split(s, -1)

	list := []string{}
	for _, t := range tokens {
		if s := strings.TrimSpace(t); s != "" {
			list = append(list, s)
		}
	}

	return strings.Join(list, "-")
}
