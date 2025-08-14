package functions

import (
	"net/netip"

	"github.com/uhppoted/uhppoted-codegen/model/requests"
	"github.com/uhppoted/uhppoted-codegen/model/responses"
)

var SetIPv4 = Function{
	Name:        "set IPv4",
	Description: "Sets the controller IPv4 address, netmask and gateway address.",
	Args: []Arg{
		{
			Name: "controller",
			Type: "controller",
		},
		{
			Name: "address",
			Type: "IPv4",
		},
		{
			Name: "netmask",
			Type: "IPv4",
		},
		{
			Name: "gateway",
			Type: "IPv4",
		},
	},
	Request:  requests.SetIPv4Request.Message,
	Response: responses.SetIPv4Response.Message,

	Tests: []FuncTest{
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
			Request: []byte{
				0x17, 0x50, 0x00, 0x00, 0x78, 0x37, 0x2a, 0x18, 0xa0, 0x7a, 0x99, 0x00, 0x20, 0x25, 0x01, 0x01,
				0x20, 0x25, 0x12, 0x31, 0x01, 0x00, 0x11, 0x01, 0x3f, 0x42, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
				0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
			},
			Replies: []TestReply{},
		},
	},
}
