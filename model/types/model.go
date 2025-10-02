package types

type Function struct {
	Name        string     `json:"name"`
	Description []string   `json:"description"`
	Args        []Arg      `json:"args"`
	Request     Message    `json:"request,omitempty"`
	Response    Message    `json:"response,omitempty"`
	Tests       []FuncTest `json:"tests,omitempty"`
}

type Request struct {
	Message
	Tests []RequestTest `json:"tests"`
}

type Response struct {
	Message     `json:"message"`
	Description []string       `json:"description"`
	Tests       []ResponseTest `json:"tests"`
}

type Message struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"msgtype"`
	Fields  []Field `json:"fields"`
}

type Field struct {
	Name        string `json:"name"`
	Type        string `json:"type"`
	Offset      uint8  `json:"offset"`
	Tag         string `json:"tag,omitempty"`
	Description string `json:"description,omitempty"`
}

type Arg struct {
	Name        string `json:"name"`
	Type        string `json:"type"`
	Description string `json:"description,omitempty"`
	Value       any    `json:"value,omitempty"`
}

type Value struct {
	Name  string `json:"name"`
	Type  string `json:"type"`
	Value any    `json:"value"`
}

type RequestTest struct {
	Name     string `json:"name"`
	Args     []Arg  `json:"args"`
	Expected []byte `json:"expected"`
}

type ResponseTest struct {
	Name     string  `json:"name"`
	Response []byte  `json:"response"`
	Expected []Value `json:"expected"`
}

type FuncTest struct {
	Name    string      `json:"name"`
	Args    []Arg       `json:"args"`
	Request []byte      `json:"request"`
	Replies []TestReply `json:"replies"`
}

type TestReply struct {
	Message  []byte  `json:"message"`
	Response []Value `json:"response"`
}
