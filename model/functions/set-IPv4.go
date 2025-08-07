package functions

import (
	"github.com/uhppoted/uhppoted-codegen/model/requests"
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
	Request: &requests.SetIPv4Request.Message,
}
