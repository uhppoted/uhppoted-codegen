package responses

var GetCardResponse = Response{
	Description: []string{
		"Container struct for the response returned from a controller when retrieving",
		"a card record stored on the controller.",
	},
	Message: Message{
		Name:    "get card response",
		MsgType: 0x5a,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "card",
				Type:        "uint32",
				Tag:         "card",
				Offset:      8,
				Description: "card number",
			},
			{
				Name:        "start date",
				Type:        "optional date",
				Tag:         "start-date",
				Offset:      12,
				Description: "'valid from' date",
			},
			{
				Name:        "end date",
				Type:        "optional date",
				Tag:         "end-date",
				Offset:      16,
				Description: "'valid until' date",
			},
			{
				Name:        "door 1",
				Type:        "uint8",
				Tag:         "door-1",
				Offset:      20,
				Description: "access permissions for door 1",
			},
			{
				Name:        "door 2",
				Type:        "uint8",
				Tag:         "door-2",
				Offset:      21,
				Description: "access permissions for door 2",
			},
			{
				Name:        "door 3",
				Type:        "uint8",
				Tag:         "door-3",
				Offset:      22,
				Description: "access permissions for door 3",
			},
			{
				Name:        "door 4",
				Type:        "uint8",
				Tag:         "door-4",
				Offset:      23,
				Description: "access permissions for door 4",
			},
			{
				Name:        "PIN",
				Type:        "pin",
				Tag:         "PIN",
				Offset:      24,
				Description: "(optional) PIN code [0..999999], 0 for none",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-card",
			Response: []byte{
				0x17, 0x5a, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa0, 0x7a, 0x99, 0x00, 0x20, 0x24, 0x01, 0x01,
				0x20, 0x24, 0x12, 0x31, 0x01, 0x00, 0x11, 0x01, 0x3f, 0x42, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "card",
					Type:  "uint32",
					Value: 10058400,
				},
				{
					Name:  "start date",
					Type:  "optional date",
					Value: "2024-01-01",
				},
				{
					Name:  "end date",
					Type:  "optional date",
					Value: "2024-12-31",
				},
				{
					Name:  "door 1",
					Type:  "uint8",
					Value: 1,
				},
				{
					Name:  "door 2",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "door 3",
					Type:  "uint8",
					Value: 17,
				},
				{
					Name:  "door 4",
					Type:  "uint8",
					Value: 1,
				},
				{
					Name:  "PIN",
					Type:  "uint32",
					Value: 999999,
				},
			},
		},
		{
			Name: "get-card-not-found",
			Response: []byte{
				0x17, 0x5a, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
					Name:  "card",
					Type:  "uint32",
					Value: 0,
				},
				{
					Name:  "start date",
					Type:  "optional date",
					Value: "0001-01-01",
				},
				{
					Name:  "end date",
					Type:  "optional date",
					Value: "0001-01-01",
				},
				{
					Name:  "door 1",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "door 2",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "door 3",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "door 4",
					Type:  "uint8",
					Value: 0,
				},
				{
					Name:  "PIN",
					Type:  "uint32",
					Value: 0,
				},
			},
		},
	},
}
