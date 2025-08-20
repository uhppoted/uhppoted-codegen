package responses

var GetListenerAddrPortResponse = Response{
	Description: []string{
		"Container struct for the response returned by a controller when retrieving",
		"the configured event listener IPv4 address and port.",
	},

	Message: Message{
		Name:    "get listener addr:port response",
		MsgType: 0x92,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "listener",
				Type:        "address:port",
				Offset:      8,
				Tag:         "listener",
				Description: "event listener IPv4 address:port",
			},
			{
				Name:        "interval",
				Type:        "uint8",
				Offset:      14,
				Tag:         "interval",
				Description: "status auto-send interval (seconds)",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-listener-address-port",
			Response: []byte{
				0x17, 0x92, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x64, 0x61, 0xea, 0x11, 0x00,
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
					Name:  "listener",
					Type:  "address:port",
					Value: "192.168.1.100:60001",
				},
				{
					Name:  "interval",
					Type:  "uint8",
					Value: "17",
				},
			},
		},
	},
}
