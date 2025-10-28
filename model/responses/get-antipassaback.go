package responses

var GetAntiPassbackResponse = Response{
	Description: []string{
		"Container struct for the response returned from to a request for the current",
		"anti-passback mode:",
		" - 0: disabled",
		" - 1: readers 1:2; 3:4 (independently)",
		" - 2: readers (1,3):(2,4)",
		" - 3: readers 1:(2,3)",
		" - 4: readers 1:(2,3,4)",
	},

	Message: Message{
		Name:    "get anti-passback response",
		MsgType: 0x86,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "antipassback",
				Type:        "anti-passback",
				Offset:      8,
				Tag:         "antipassback",
				Description: "anti-passback mode",
			},
		},
	},

	Tests: []ResponseTest{
		{
			Name: "get-anti-passback",
			Response: []byte{
				0x17, 0x86, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "antipassback",
					Type:  "anti-passback",
					Value: 2,
				},
			},
		},
	},
}
