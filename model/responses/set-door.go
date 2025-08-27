package responses

var SetDoorResponse = Response{
	Description: []string{
		"Container struct for the response returned by a controller after updating",
		"a door configuration.",
	},
	Message: Message{
		Name:    "set door response",
		MsgType: 0x80,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:   "door",
				Type:   "uint8",
				Offset: 8,
				Tag:    "door",
			},
			{
				Name:   "mode",
				Type:   "uint8",
				Offset: 9,
				Tag:    "mode",
			},
			{
				Name:   "delay",
				Type:   "uint8",
				Offset: 10,
				Tag:    "delay",
			},
		},
	},

	Tests: []ResponseTest{
		{
			Name: "set-door",
			Response: []byte{
				0x17, 0x80, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x02, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
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
