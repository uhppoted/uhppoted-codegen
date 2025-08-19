package responses

var GetControllerResponse = Response{
	Description: []string{
		"Container struct for the response returned from an access controller when retrieving the",
		"network configuration, firmware version and firmware release date.",
	},

	Message: Message{
		Name:    "get controller response",
		MsgType: 0x94,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "ip address",
				Type:        "IPv4",
				Offset:      8,
				Tag:         "ip-address",
				Description: "controller IPv4 address, e.g. 192.168.1.100",
			},
			{
				Name:        "subnet mask",
				Type:        "IPv4",
				Offset:      12,
				Tag:         "netmask",
				Description: "controller IPv4 netmask, e.g. 255.255.255.0",
			},
			{
				Name:        "gateway",
				Type:        "IPv4",
				Offset:      16,
				Tag:         "gateway",
				Description: "controller IPv4 gateway address, e.g. 192.168.1.1",
			},
			{
				Name:        "MAC address",
				Type:        "MAC",
				Offset:      20,
				Tag:         "MAC",
				Description: "controller MAC address, e.g. 52:fd:fc:07:21:82",
			},
			{
				Name:        "version",
				Type:        "version",
				Offset:      26,
				Tag:         "version",
				Description: "controller firmware version, e.g. v6.62",
			},
			{
				Name:        "date",
				Type:        "date",
				Offset:      28,
				Tag:         "date",
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
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "ip address",
					Type:  "IPv4",
					Value: "192.168.1.100",
				},
				{
					Name:  "subnet mask",
					Type:  "IPv4",
					Value: "255.255.255.0",
				},
				{
					Name:  "gateway",
					Type:  "IPv4",
					Value: "192.168.1.1",
				},
				{
					Name:  "MAC address",
					Type:  "MAC",
					Value: "00:12:23:34:45:56",
				},
				{
					Name:  "version",
					Type:  "version",
					Value: "v8.92",
				},
				{
					Name:  "date",
					Type:  "date",
					Value: "2018-11-05",
				},
			},
		},
	},
}
