package codegen

type function struct {
	Name     string
	Args     []arg
	Request  *request
	Response *response
}

type request struct {
	Name    string
	MsgType uint8
	Fields  []field
}

type response struct {
	Name    string  `json:"name"`
	MsgType uint8   `json:"type"`
	Fields  []field `json:"fields"`
}

type arg struct {
	Name string
	Type string
}

type field struct {
	Name   string
	Type   string
	Offset uint8
}
