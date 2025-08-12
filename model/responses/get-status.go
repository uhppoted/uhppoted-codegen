package responses

var GetStatusResponse = Response{
	Message: Message{
		Name:    "get status response",
		MsgType: 0x20,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:        "system date",
				Type:        "shortdate",
				Offset:      51,
				Description: "controller system date, e.g. 2025-07-21",
			},
			{
				Name:        "system time",
				Type:        "time",
				Offset:      37,
				Description: "controller system time, e.g. 13:25:47",
			},
			{
				Name:        "door 1 open",
				Type:        "bool",
				Offset:      28,
				Description: "door 1 open sensor",
			},
			{
				Name:        "door 2 open",
				Type:        "bool",
				Offset:      29,
				Description: "door 2 open sensor",
			},
			{
				Name:        "door 3 open",
				Type:        "bool",
				Offset:      30,
				Description: "door 3 open sensor",
			},
			{
				Name:        "door 4 open",
				Type:        "bool",
				Offset:      31,
				Description: "door 4 open sensor",
			},
			{
				Name:        "door 1 button",
				Type:        "bool",
				Offset:      32,
				Description: "door 1 button pressed",
			},
			{
				Name:        "door 2 button",
				Type:        "bool",
				Offset:      33,
				Description: "door 2 button pressed",
			},
			{
				Name:        "door 3 button",
				Type:        "bool",
				Offset:      34,
				Description: "door 3 button pressed",
			},
			{
				Name:        "door 4 button",
				Type:        "bool",
				Offset:      35,
				Description: "door 4 button pressed",
			},
			{
				Name:        "relays",
				Type:        "uint8",
				Offset:      49,
				Description: "bitset of door unlock relay states",
			},
			{
				Name:        "inputs",
				Type:        "uint8",
				Offset:      50,
				Description: "bitset of alarm inputs",
			},
			{
				Name:        "system error",
				Type:        "uint8",
				Offset:      36,
				Description: "system error code",
			},
			{
				Name:        "special info",
				Type:        "uint8",
				Offset:      48,
				Description: "absolutely no idea",
			},
			{
				Name:        "event index",
				Type:        "uint32",
				Offset:      8,
				Description: "last event index",
			},
			{
				Name:        "event type",
				Type:        "uint8",
				Offset:      12,
				Description: "last event type",
			},
			{
				Name:        "event access granted",
				Type:        "bool",
				Offset:      13,
				Description: "last event access granted",
			},
			{
				Name:        "event door",
				Type:        "uint8",
				Offset:      14,
				Description: "last event door",
			},
			{
				Name:        "event direction",
				Type:        "uint8",
				Offset:      15,
				Description: "last event door direction (0: in, 1: out)",
			},
			{
				Name:        "event card",
				Type:        "uint32",
				Offset:      16,
				Description: "last event card number",
			},
			{
				Name:        "event timestamp",
				Type:        "optional datetime",
				Offset:      20,
				Description: "last event timestamp",
			},
			{
				Name:        "event reason",
				Type:        "uint8",
				Offset:      27,
				Description: "last event reason",
			},
			{
				Name:        "sequence no",
				Type:        "uint32",
				Offset:      40,
				Description: "packet sequence number",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-status",
			Response: []byte{
				0x17, 0x20, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x4e, 0x00, 0x00, 0x00, 0x02, 0x01, 0x03, 0x01,
				0xa1, 0x98, 0x7c, 0x00, 0x20, 0x22, 0x08, 0x23, 0x09, 0x47, 0x06, 0x2c, 0x00, 0x01, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x01, 0x03, 0x09, 0x49, 0x39, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x27, 0x07, 0x09, 0x22, 0x08, 0x23, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Expected: []Value{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "system-date",
					Type:  "date",
					Value: "2022-08-23",
				},
				{
					Name:  "system-time",
					Type:  "time",
					Value: "09:49:39",
				},
				{
					Name:  "door-1-open",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door-2-open",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "door-3-open",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door-4-open",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door-1-button",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door-2-button",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door-3-button",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "door-4-button",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "relays",
					Type:  "uint8",
					Value: 0x07,
				},
				{
					Name:  "inputs",
					Type:  "uint8",
					Value: 0x09,
				},
				{
					Name:  "system-error",
					Type:  "uint8",
					Value: 3,
				},
				{
					Name:  "special-info",
					Type:  "uint8",
					Value: 39,
				},
				{
					Name:  "event-index",
					Type:  "uint32",
					Value: 78,
				},
				{
					Name:  "event-type",
					Type:  "uint8",
					Value: 2,
				},
				{
					Name:  "event-access-granted",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "event-door",
					Type:  "uint8",
					Value: 3,
				},
				{
					Name:  "event-direction",
					Type:  "uint8",
					Value: 1,
				},
				{
					Name:  "event-card",
					Type:  "uint32",
					Value: 8165537,
				},
				{
					Name:  "event-timestamp",
					Type:  "datetime",
					Value: "2022-08-23 09:47:06",
				},
				{
					Name:  "event-reason",
					Type:  "uint8",
					Value: 44,
				},
				{
					Name:  "sequence-no",
					Type:  "uint32",
					Value: 0,
				},
			},
		},
	},
}
