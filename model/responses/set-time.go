package responses

var SetTimeResponse = Response{
	Description: []string{
		"Container struct for the response returned by a controller after setting the system date/time.",
	},
	Message: Message{
		Name:    "set time response",
		MsgType: 0x30,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "date-time",
				Type:        "datetime",
				Offset:      8,
				Tag:         "date-time",
				Description: "controller system date/time",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "set-time",
			Response: []byte{
				0x17, 0x30, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x20, 0x25, 0x11, 0x01, 0x12, 0x34, 0x56, 0x00,
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
					Name:  "date-time",
					Type:  "datetime",
					Value: "2025-11-01 12:34:56",
				},
			},
		},
	},
}
