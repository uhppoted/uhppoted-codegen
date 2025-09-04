package requests

var ActivateKeypadsRequest = Request{
	Message: Message{
		Name:    "activate keypads request",
		MsgType: 0xa4,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:   "reader 1",
				Type:   "bool",
				Offset: 8,
			},
			{
				Name:   "reader 2",
				Type:   "bool",
				Offset: 9,
			},
			{
				Name:   "reader 3",
				Type:   "bool",
				Offset: 10,
			},
			{
				Name:   "reader 4",
				Type:   "bool",
				Offset: 11,
			},
		},
	},

	Tests: []RequestTest{
		{
			Name: "activate-keypads",
			Args: []TestArg{
				{
					Arg: Arg{
						Name: "controller",
						Type: "uint32",
					},
					Value: 405419896,
				},
				{
					Arg: Arg{
						Name: "reader 1",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "reader 2",
						Type: "bool",
					},
					Value: true,
				},
				{
					Arg: Arg{
						Name: "reader 3",
						Type: "bool",
					},
					Value: false,
				},
				{
					Arg: Arg{
						Name: "reader 4",
						Type: "bool",
					},
					Value: true,
				},
			},
			Expected: []byte{
				0x17, 0xa4, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
