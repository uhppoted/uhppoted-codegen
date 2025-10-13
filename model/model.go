package model

import (
	"github.com/uhppoted/uhppoted-codegen/model/types"
)

type Model struct {
	Functions []types.Function `json:"functions"`
	Requests  []types.Message  `json:"requests"`
	Responses []types.Message  `json:"responses"`
	Event     types.Message    `json:"event"`
}

type TestData struct {
	Tests []Test `json:"tests"`
}

type Arg struct {
	Name        string `json:"name"`
	Type        string `json:"type"`
	Description string `json:"description"`
}

type TestArg struct {
	Arg
	Value any `json:"value"`
}

type Value struct {
	Name  string `json:"name"`
	Type  string `json:"type"`
	Value any    `json:"value"`
}

type Test struct {
	Name     string        `json:"name"`
	Request  *TestRequest  `json:"request,omitempty"`
	Response *TestResponse `json:"response,omitempty"`
}

type TestRequest struct {
	Name    string  `json:"name"`
	Values  []Value `json:"values"`
	Message []uint8 `json:"message,omitempty"`
}

type TestResponse struct {
	Name    string  `json:"name"`
	Values  []Value `json:"values"`
	Message []uint8 `json:"message,omitempty"`
}

var Event = types.Message{
	Name:    "event",
	MsgType: 0x20,
	Fields: []types.Field{
		types.Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		types.Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
		types.Field{
			Name:   "event type",
			Type:   "uint8",
			Offset: 12,
		},
		types.Field{
			Name:   "event access granted",
			Type:   "bool",
			Offset: 13,
		},
		types.Field{
			Name:   "event door",
			Type:   "uint8",
			Offset: 14,
		},
		types.Field{
			Name:   "event direction",
			Type:   "direction",
			Offset: 15,
		},
		types.Field{
			Name:   "event card",
			Type:   "uint32",
			Offset: 16,
		},
		types.Field{
			Name:   "event timestamp",
			Type:   "datetime",
			Offset: 20,
		},
		types.Field{
			Name:   "event reason",
			Type:   "uint8",
			Offset: 27,
		},
		types.Field{
			Name:   "system date",
			Type:   "shortdate",
			Offset: 51,
		},
		types.Field{
			Name:   "system time",
			Type:   "time",
			Offset: 37,
		},
		types.Field{
			Name:   "system error",
			Type:   "uint8",
			Offset: 36,
		},
		types.Field{
			Name:   "door 1 open",
			Type:   "bool",
			Offset: 28,
		},
		types.Field{
			Name:   "door 2 open",
			Type:   "bool",
			Offset: 29,
		},
		types.Field{
			Name:   "door 3 open",
			Type:   "bool",
			Offset: 30,
		},
		types.Field{
			Name:   "door 4 open",
			Type:   "bool",
			Offset: 31,
		},
		types.Field{
			Name:   "door 1 button",
			Type:   "bool",
			Offset: 32,
		},
		types.Field{
			Name:   "door 2 button",
			Type:   "bool",
			Offset: 33,
		},
		types.Field{
			Name:   "door 3 button",
			Type:   "bool",
			Offset: 34,
		},
		types.Field{
			Name:   "door 4 button",
			Type:   "bool",
			Offset: 35,
		},
		types.Field{
			Name:   "relays",
			Type:   "uint8",
			Offset: 49,
		},
		types.Field{
			Name:   "inputs",
			Type:   "uint8",
			Offset: 50,
		},
		types.Field{
			Name:   "special info",
			Type:   "uint8",
			Offset: 48,
		},
		types.Field{
			Name:   "sequence no",
			Type:   "uint32",
			Offset: 40,
		},
	},
}
