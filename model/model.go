package model

type Model struct {
	Functions []Function `json:"functions"`
	Requests  []Request  `json:"requests"`
	Responses []Response `json:"responses"`
}

type Function struct {
	Name     string    `json:"name"`
	Args     []Arg     `json:"args"`
	Request  *Request  `json:"request,omitempty"`
	Response *Response `json:"response,omitempty"`
}

type Request struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"type"`
	Fields  []Field `json:"fields"`
}

type Response struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"type"`
	Fields  []Field `json:"fields"`
}

type Arg struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type Field struct {
	Name   string `json:"name"`
	Type   string `json:"type"`
	Offset uint8  `json:"offset"`
}
