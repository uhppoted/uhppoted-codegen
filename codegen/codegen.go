package codegen

type arg struct {
	Name string
	Type string
}

type field struct {
	Name   string
	Type   string
	Offset uint8
}

var DeviceID = field{
	Name:   "device id",
	Type:   "uint32",
	Offset: 4,
}
