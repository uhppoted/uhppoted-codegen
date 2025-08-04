package types

// type TestData struct {
//     Tests []Test `json:"tests"`
// }

// type Function struct {
//     Name     string   `json:"name"`
// Description string `json:"description"`
//     Args     []Arg    `json:"args"`
//     Request  *Message `json:"request,omitempty"`
//     Response *Message `json:"response,omitempty"`
// }

type Message struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"msgtype"`
	Fields  []Field `json:"fields"`
}

type Field struct {
	Name        string `json:"name"`
	Type        string `json:"type"`
	Offset      uint8  `json:"offset"`
	Description string `json:"description,omitempty"`
}

type Arg struct {
	Name string `json:"name"`
	Type string `json:"type"`
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

type TestArg struct {
	Arg
	Value any `json:"value"`
}

// type Value struct {
//     Name  string `json:"name"`
//     Type  string `json:"type"`
//     Value any    `json:"value"`
// }

// type Test struct {
//     Name     string        `json:"name"`
//     Request  *TestRequest  `json:"request,omitempty"`
//     Response *TestResponse `json:"response,omitempty"`
// }

// type TestRequest struct {
//     Name    string  `json:"name"`
//     Values  []Value `json:"values"`
//     Message []uint8 `json:"message,omitempty"`
// }

// type TestResponse struct {
//     Name    string  `json:"name"`
//     Values  []Value `json:"values"`
//     Message []uint8 `json:"message,omitempty"`
// }
