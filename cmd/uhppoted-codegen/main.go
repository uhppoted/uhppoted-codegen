package main

import (
	"fmt"
	"os"

	core "github.com/uhppoted/uhppote-core/uhppote"
	lib "github.com/uhppoted/uhppoted-lib/command"

	"github.com/uhppoted/uhppoted-codegen/commands"
)

var cli = []lib.Command{
	&commands.GENERATE,
	&commands.REGENERATE,
	&lib.Version{
		Application: commands.APPLICATION,
		Version:     core.VERSION,
	},
}

var help = lib.NewHelp(commands.APPLICATION, cli, &commands.GENERATE)

func main() {
	cmd, err := lib.Parse(cli, &commands.GENERATE, help)
	if err != nil {
		fmt.Printf("\nError parsing command line: %v\n\n", err)
		os.Exit(1)
	}

	if err = cmd.Execute(); err != nil {
		fmt.Printf("\nERROR: %v\n\n", err)
		os.Exit(1)
	}
}
