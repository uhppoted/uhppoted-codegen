package responses

var GetTimeProfileResponse = Response{
	Description: []string{
		"Container struct for the response returned from an access controller when retrieving",
		"an access time profile.",
	},

	Message: Message{
		Name:    "get time profile response",
		MsgType: 0x98,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Tag:         "controller",
				Description: "controller serial number",
			},
			{
				Name:        "profile",
				Type:        "uint8",
				Tag:         "profile",
				Offset:      8,
				Description: "profile ID [2..254]",
			},
			{
				Name:        "start date",
				Type:        "optional date",
				Offset:      9,
				Tag:         "start-date",
				Description: "date from which profile is valid (inclusive)",
			},
			{
				Name:        "end date",
				Type:        "optional date",
				Offset:      13,
				Tag:         "end-date",
				Description: "date after which profile is invalid",
			},
			{
				Name:        "monday",
				Type:        "bool",
				Offset:      17,
				Tag:         "monday",
				Description: "profile enabled on Monday if true",
			},
			{
				Name:        "tuesday",
				Type:        "bool",
				Offset:      18,
				Tag:         "tuesday",
				Description: "profile enabled on Tuesday if true",
			},
			{
				Name:        "wednesday",
				Type:        "bool",
				Offset:      19,
				Tag:         "wednesday",
				Description: "profile enabled on Wednesday if true",
			},
			{
				Name:        "thursday",
				Type:        "bool",
				Offset:      20,
				Tag:         "thursday",
				Description: "profile enabled on Thursday if true",
			},
			{
				Name:        "friday",
				Type:        "bool",
				Offset:      21,
				Tag:         "friday",
				Description: "profile enabled on Monday if true",
			},
			{
				Name:        "saturday",
				Type:        "bool",
				Offset:      22,
				Tag:         "saturday",
				Description: "profile enabled on Friday if true",
			},
			{
				Name:        "sunday",
				Type:        "bool",
				Offset:      23,
				Tag:         "sunday",
				Description: "profile enabled on Sunday if true",
			},
			{
				Name:        "segment 1 start",
				Type:        "HHmm",
				Offset:      24,
				Tag:         "segment1-start",
				Description: "start time for first time segment",
			},
			{
				Name:        "segment 1 end",
				Type:        "HHmm",
				Offset:      26,
				Tag:         "segment1-end",
				Description: "end time for first time segment",
			},
			{
				Name:        "segment 2 start",
				Type:        "HHmm",
				Offset:      28,
				Tag:         "segment2-start",
				Description: "start time for second time segment",
			},
			{
				Name:        "segment 2 end",
				Type:        "HHmm",
				Offset:      30,
				Tag:         "segment2-end",
				Description: "end time for second time segment",
			},
			{
				Name:        "segment 3 start",
				Type:        "HHmm",
				Offset:      32,
				Tag:         "segment3-start",
				Description: "start time for third time segment",
			},
			{
				Name:        "segment 3 end",
				Type:        "HHmm",
				Offset:      34,
				Tag:         "segment3-end",
				Description: "end time for third time segment",
			},
			{
				Name:        "linked profile",
				Type:        "uint8",
				Offset:      36,
				Tag:         "linked-profile",
				Description: "ID of linked profile (0 if not linked)",
			},
		},
	},
	Tests: []ResponseTest{
		{
			Name: "get-time-profile",
			Response: []byte{
				0x17, 0x98, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0x25, 0x20, 0x25, 0x11, 0x26, 0x20, 0x25, 0x12,
				0x29, 0x01, 0x01, 0x00, 0x01, 0x00, 0x01, 0x01, 0x08, 0x30, 0x09, 0x45, 0x11, 0x35, 0x13, 0x15,
				0x14, 0x01, 0x17, 0x59, 0x13, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Expected: []Value{
				{
					Name:  "controller",
					Type:  "uint32",
					Value: 405419896,
				},
				{
					Name:  "profile",
					Type:  "uint8",
					Value: 37,
				},
				{
					Name:  "start date",
					Type:  "optional date",
					Value: "2025-11-26",
				},
				{
					Name:  "end date",
					Type:  "optional date",
					Value: "2025-12-29",
				},
				{
					Name:  "monday",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "tuesday",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "wednesday",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "thursday",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "friday",
					Type:  "bool",
					Value: false,
				},
				{
					Name:  "saturday",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "sunday",
					Type:  "bool",
					Value: true,
				},
				{
					Name:  "segment 1 start",
					Type:  "HHmm",
					Value: "08:30",
				},
				{
					Name:  "segment 1 end",
					Type:  "HHmm",
					Value: "09:45",
				},
				{
					Name:  "segment 2 start",
					Type:  "HHmm",
					Value: "11:35",
				},
				{
					Name:  "segment 2 end",
					Type:  "HHmm",
					Value: "13:15",
				},
				{
					Name:  "segment 3 start",
					Type:  "HHmm",
					Value: "14:01",
				},
				{
					Name:  "segment 3 end",
					Type:  "HHmm",
					Value: "17:59",
				},
				{
					Name:  "linked profile",
					Type:  "uint8",
					Value: 19,
				},
			},
		},
	},
}
