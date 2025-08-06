package requests

import (
	"net/netip"
)

var SetIPv4Request = Request{
	Message: Message{
		Name:    "set IPv4 request",
		MsgType: 0x96,
		Fields: []Field{
			{
				Name:        "controller",
				Type:        "uint32",
				Offset:      4,
				Description: "controller serial number",
			},
			{
				Name:        "address",
				Type:        "IPv4",
				Offset:      8,
				Description: "controller IPv4 address",
			},
			{
				Name:        "netmask",
				Type:        "IPv4",
				Offset:      12,
				Description: "controller IPv4 subnet mask",
			},
			{
				Name:        "gateway",
				Type:        "IPv4",
				Offset:      16,
				Description: "controller IPv4 gateway address",
			},
			{
				Name:        "",
				Type:        "magic",
				Offset:      20,
				Description: "'magic' word",
			},
		},
	},
	Tests: []RequestTest{
		{
			Name: "set-IPv4",
			Args: []TestArg{
				{
					Arg: Arg{
						Name: "controller",
						Type: "uint32",
					},
					Value: uint32(405419896),
				},
				{
					Arg: Arg{
						Name: "address",
						Type: "IPv4",
					},
					Value: netip.MustParseAddr("192.168.1.125"),
				},
				{
					Arg: Arg{
						Name: "netmask",
						Type: "IPv4",
					},
					Value: netip.MustParseAddr("255.255.255.0"),
				},
				{
					Arg: Arg{
						Name: "gateway",
						Type: "IPv4",
					},
					Value: netip.MustParseAddr("192.168.1.1"),
				},
			},
			Expected: []byte{
				0x17, 0x96, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xc0, 0xa8, 0x01, 0x7d, 0xff, 0xff, 0xff, 0x00,
				0xc0, 0xa8, 0x01, 0x01, 0x55, 0xaa, 0xaa, 0x55, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
		},
	},
}
