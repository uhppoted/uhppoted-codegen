package responses

var GetDoorResponse = Response{
	Message: Message{
		Name:    "get door response",
		MsgType: 0x82,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "door",
				Type:        "uint8",
				Offset:      8,
				Tag:         "door",
				Description: "door ID ([1..4]",
			},
			{
				Name:        "mode",
				Type:        "uint8",
				Offset:      9,
				Tag:         "mode",
				Description: "control mode (1:normally open, 2:normally closed. 3:controlled)",
			},
			{
				Name:        "delay",
				Type:        "uint8",
				Offset:      10,
				Tag:         "delay",
				Description: "unlock delay (seconds)",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-door",
			Response: []byte{
				0x17, 0x82, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x02, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "door",
					Type:  "uint8",
					Value: 3,
				},
				{
					Name:  "mode",
					Type:  "uint8",
					Value: 2,
				},
				{
					Name:  "delay",
					Type:  "uint8",
					Value: 7,
				},
			},
		},
	},
}
