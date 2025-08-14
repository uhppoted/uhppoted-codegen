package requests

var SetDoorPasscodesRequest = Request{
	Message: Message{
		Name:    "set door passcodes request",
		MsgType: 0x8c,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "door",
				Type:   "uint8",
				Offset: 8,
			},
			{
				Name:   "passcode 1",
				Type:   "pin",
				Offset: 12,
			},
			{
				Name:   "passcode 2",
				Type:   "pin",
				Offset: 16,
			},
			{
				Name:   "passcode 3",
				Type:   "pin",
				Offset: 20,
			},
			{
				Name:   "passcode 4",
				Type:   "pin",
				Offset: 24,
			},
		},
	},

	Tests: []RequestTest{
		{
			Name: "set-door-passcodes",
			Args: []TestArg{
				{
					Arg: Arg{
						Name: "controller",
						Type: "uint32",
					}, Value: 405419896,
				},
				{
					Arg: Arg{
						Name: "door",
						Type: "uint8",
					},
					Value: 3,
				},
				{
					Arg: Arg{
						Name: "passcode 1",
						Type: "uint32",
					},
					Value: 123456,
				},
				{
					Arg: Arg{
						Name: "passcode 2",
						Type: "uint32",
					},
					Value: 234567,
				},
				{
					Arg: Arg{
						Name: "passcode 3",
						Type: "uint32",
					},
					Value: 345678,
				},
				{
					Arg: Arg{
						Name: "passcode 4",
						Type: "uint32",
					},
					Value: 456789,
				},
			},
			Expected: []byte{
				0x17, 0x8c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x00, 0x00, 0x00, 0x40, 0xe2, 0x01, 0x00,
				0x47, 0x94, 0x03, 0x00, 0x4e, 0x46, 0x05, 0x00, 0x55, 0xf8, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
		{
			Name: "set-door-passcodes-with-invalid-passcode",
			Args: []TestArg{
				{
					Arg: Arg{
						Name: "controller",
						Type: "uint32",
					}, Value: 405419896,
				},
				{
					Arg: Arg{
						Name: "door",
						Type: "uint8",
					},
					Value: 3,
				},
				{
					Arg: Arg{
						Name: "passcode 1",
						Type: "uint32",
					},
					Value: 123456,
				},
				{
					Arg: Arg{
						Name: "passcode 2",
						Type: "uint32",
					},
					Value: 1234567,
				},
				{
					Arg: Arg{
						Name: "passcode 3",
						Type: "uint32",
					},
					Value: 345678,
				},
				{
					Arg: Arg{
						Name: "passcode 4",
						Type: "uint32",
					},
					Value: 456789,
				},
			},
			Expected: []byte{
				0x17, 0x8c, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x03, 0x00, 0x00, 0x00, 0x40, 0xe2, 0x01, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x4e, 0x46, 0x05, 0x00, 0x55, 0xf8, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
