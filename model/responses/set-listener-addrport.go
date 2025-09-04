package responses

var SetListenerAddrPortResponse = Response{
	Description: []string{
		"Container struct for the response returned by a controller when setting",
		"the event listener IPv4 address and port.",
	},

	Message: Message{
		Name:    "set listener addr:port response",
		MsgType: 0x90,
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
			Name: "set-listener-address:port",
			Response: []byte{
				0x17, 0x90, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
