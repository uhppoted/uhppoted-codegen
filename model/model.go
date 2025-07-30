package model

type Model struct {
	Functions []Function `json:"functions"`
	Requests  []Message  `json:"requests"`
	Responses []Message  `json:"responses"`
	Event     Message    `json:"event"`
}

type TestData struct {
	Tests []Test `json:"tests"`
}

type Function struct {
	Name     string   `json:"name"`
	Args     []Arg    `json:"args"`
	Request  *Message `json:"request,omitempty"`
	Response *Message `json:"response,omitempty"`
}

type Message struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"msgtype"`
	Fields  []Field `json:"fields"`
}

type Arg struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type TestArg struct {
	Arg
	Value any `json:"value"`
}

type Field struct {
	Name        string `json:"name"`
	Type        string `json:"type"`
	Offset      uint8  `json:"offset"`
	Description string `json:"description,omitempty"`
}

type Request struct {
	Message
	Tests []RequestTest
}

type RequestTest struct {
	Name     string    `json:"name"`
	Args     []TestArg `json:"args"`
	Expected []byte    `json:"expected"`
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

var Event = Message{
	Name:    "event",
	MsgType: 0x20,
	Fields: []Field{
		Field{
			Name:   "device id",
			Type:   "uint32",
			Offset: 4,
		},
		Field{
			Name:   "event index",
			Type:   "uint32",
			Offset: 8,
		},
		Field{
			Name:   "event type",
			Type:   "uint8",
			Offset: 12,
		},
		Field{
			Name:   "event access granted",
			Type:   "bool",
			Offset: 13,
		},
		Field{
			Name:   "event door",
			Type:   "uint8",
			Offset: 14,
		},
		Field{
			Name:   "event direction",
			Type:   "uint8",
			Offset: 15,
		},
		Field{
			Name:   "event card",
			Type:   "uint32",
			Offset: 16,
		},
		Field{
			Name:   "event timestamp",
			Type:   "datetime",
			Offset: 20,
		},
		Field{
			Name:   "event reason",
			Type:   "uint8",
			Offset: 27,
		},
		Field{
			Name:   "system date",
			Type:   "shortdate",
			Offset: 51,
		},
		Field{
			Name:   "system time",
			Type:   "time",
			Offset: 37,
		},
		Field{
			Name:   "system error",
			Type:   "uint8",
			Offset: 36,
		},
		Field{
			Name:   "door 1 open",
			Type:   "bool",
			Offset: 28,
		},
		Field{
			Name:   "door 2 open",
			Type:   "bool",
			Offset: 29,
		},
		Field{
			Name:   "door 3 open",
			Type:   "bool",
			Offset: 30,
		},
		Field{
			Name:   "door 4 open",
			Type:   "bool",
			Offset: 31,
		},
		Field{
			Name:   "door 1 button",
			Type:   "bool",
			Offset: 32,
		},
		Field{
			Name:   "door 2 button",
			Type:   "bool",
			Offset: 33,
		},
		Field{
			Name:   "door 3 button",
			Type:   "bool",
			Offset: 34,
		},
		Field{
			Name:   "door 4 button",
			Type:   "bool",
			Offset: 35,
		},
		Field{
			Name:   "relays",
			Type:   "uint8",
			Offset: 49,
		},
		Field{
			Name:   "inputs",
			Type:   "uint8",
			Offset: 50,
		},
		Field{
			Name:   "special info",
			Type:   "uint8",
			Offset: 48,
		},
		Field{
			Name:   "sequence no",
			Type:   "uint32",
			Offset: 40,
		},
	},
}
