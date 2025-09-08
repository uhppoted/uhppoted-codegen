package responses

var GetCardsResponse = Response{
	Description: []string{
		"Container struct for the response returned from a controller when retrieving the number of",
		"cards stored on the controller.",
	},
	Message: Message{
		Name:    "get cards response",
		MsgType: 0x58,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "cards",
				Type:        "uint32",
				Tag:         "cards",
				Offset:      8,
				Description: "number of stored cards",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-cards",
			Response: []byte{
				0x17, 0x58, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x0b, 0x35, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "cards",
					Type:  "uint32",
					Value: 13579,
				},
			},
		},
	},
}
