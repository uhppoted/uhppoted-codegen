package codegen

type function struct {
	Name     string    `json:"name"`
	Args     []arg     `json:"args"`
	Request  *request  `json:"request,omitempty"`
	Response *response `json:"response,omitempty"`
}

type request struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"type"`
	Fields  []field `json:"fields"`
}

type response struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"type"`
	Fields  []field `json:"fields"`
}

type arg struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

type field struct {
	Name   string `json:"name"`
	Type   string `json:"type"`
	Offset uint8  `json:"offset"`
}
