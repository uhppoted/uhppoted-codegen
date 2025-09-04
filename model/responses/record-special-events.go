package responses

var RecordSpecialEventsResponse = Response{
	Description: []string{
		"Container struct for the response returned from an access controller when enabling or",
		"disabling events for door opened, door closed and button pressed.",
	},

	Message: Message{
		Name:    "record special events response",
		MsgType: 0x8e,
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
			Name: "record-special-events",
			Response: []byte{
				0x17, 0x8e, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
