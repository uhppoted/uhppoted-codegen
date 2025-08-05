package responses

var GetControllerResponse = Response{
	Message: Message{
		Name:    "get controller response",
		MsgType: 0x94,
		Fields: []Field{
			Field{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			Field{
				Name:        "ip address",
				Type:        "IPv4",
				Offset:      8,
				Description: "controller IPv4 address, e.g. 192.168.1.100",
			},
			Field{
				Name:        "subnet mask",
				Type:        "IPv4",
				Offset:      12,
				Description: "controller IPv4 netmask, e.g. 255.255.255.0",
			},
			Field{
				Name:        "gateway",
				Type:        "IPv4",
				Offset:      16,
				Description: "controller IPv4 gateway address, e.g. 192.168.1.1",
			},
			Field{
				Name:        "MAC address",
				Type:        "MAC",
				Offset:      20,
				Description: "controller MAC address, e.g. 52:fd:fc:07:21:82",
			},
			Field{
				Name:        "version",
				Type:        "version",
				Offset:      26,
				Description: "controller firmware version, e.g. v6.62",
			},
			Field{
				Name:        "date",
				Type:        "date",
				Offset:      28,
				Description: "controller firmware release date, e.g. 2020-12-31",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-controller",
			Response: []byte{
				0x17, 0x94, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x64, 0xff, 0xff, 0xff, 0x00,
				0xc0, 0xa8, 0x01, 0x01, 0x00, 0x12, 0x23, 0x34, 0x45, 0x56, 0x08, 0x92, 0x20, 0x18, 0x11, 0x05,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Expected: []Value{
				Value{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				Value{
					Name:  "ip address",
					Type:  "IPv4",
					Value: "192.168.1.100",
				},
				Value{
					Name:  "subnet mask",
					Type:  "IPv4",
					Value: "255.255.255.0",
				},
				Value{
					Name:  "gateway",
					Type:  "IPv4",
					Value: "192.168.1.1",
				},
				Value{
					Name:  "MAC address",
					Type:  "MAC",
					Value: "00:12:23:34:45:56",
				},
				Value{
					Name:  "version",
					Type:  "version",
					Value: "v8.92",
				},
				Value{
					Name:  "date",
					Type:  "date",
					Value: "2018-11-05",
				},
			},
		},
	},
}
