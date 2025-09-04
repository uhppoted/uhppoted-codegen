package responses

var SetIPv4Response = Response{
	Description: []string{
		"SetIPv4Response is a synthesized response provided to simplify code generation. The controller",
		"does not return a response to a 'set-IPv4' request.",
	},

	Message: Message{
		Name:    "set IPv4 response",
		MsgType: 0x96,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "ok",
				Type:        "bool",
				Offset:      8,
				Tag:         "ok",
				Description: "succeeded/failed",
			},
		},
	},

	Tests: []ResponseTest{
		{
			Name: "set-IPv4",
			Response: []byte{
				0x17, 0x96, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Expected: []Value{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "ok",
					Type:  "bool",
					Value: true,
				},
			},
		},
	},
}
