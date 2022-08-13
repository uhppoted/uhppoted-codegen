package commands

import (
	"flag"
	"fmt"
	"log"
	"os"

	core "github.com/uhppoted/uhppote-core/uhppote"

	"github.com/uhppoted/uhppoted-codegen/codegen"
)

type Generate struct {
	models    string
	templates string
	out       string
	clean     bool
	debug     bool
}

var GENERATE = Generate{
	debug: false,
}

func (cmd *Generate) flags() *flag.FlagSet {
	flagset := flag.NewFlagSet("generate", flag.ExitOnError)

	flagset.StringVar(&cmd.models, "models", "", "folder containing the code generator model JSON files")
	flagset.StringVar(&cmd.templates, "templates", "", "folder containing the language templates for the code generator")
	flagset.StringVar(&cmd.out, "out", "", "output folder for the generated native code UHPPOTE interface")
	flagset.BoolVar(&cmd.clean, "clean", false, "deletes all existing files and folders from the output folder")
	flagset.BoolVar(&cmd.debug, "debug", false, "Enables verbose debugging information")

	return flagset
}

func (cmd *Generate) Name() string {
	return "generate"
}

func (cmd *Generate) Description() string {
	return "Runs the uhppoted-codegen against a set of language templates to create a native language interface to the UHPPOTE controllers"
}

func (cmd *Generate) Usage() string {
	return "uhppoted-codegen [--debug] --templates <folder> --out <folder>"
}

func (cmd *Generate) Help() {
	fmt.Println()
	fmt.Println("  Usage: uhppoted-codegen generate --models <folder> --templates <folder> --out <folder> --debug")
	fmt.Println()
	fmt.Println("  Options:")
	fmt.Println()
	cmd.FlagSet().VisitAll(func(f *flag.Flag) {
		fmt.Printf("    --%-12s %s\n", f.Name, f.Usage)
	})
	fmt.Println()
}

func (cmd *Generate) FlagSet() *flag.FlagSet {
	return cmd.flags()
}

func (cmd *Generate) Execute(args ...interface{}) error {
	log.Printf("%s %s (PID %d)\n", APPLICATION, core.VERSION, os.Getpid())

	if cmd.models == "" {
		return fmt.Errorf("missing 'models' folder")
	} else if _, err := os.Stat(cmd.models); err != nil && os.IsNotExist(err) {
		return fmt.Errorf("models folder '%v' does not exist", cmd.models)
	} else if err != nil {
		return err
	}

	if cmd.templates == "" {
		return fmt.Errorf("missing 'templates' folder")
	} else if _, err := os.Stat(cmd.templates); err != nil && os.IsNotExist(err) {
		return fmt.Errorf("templates folder '%v' does not exist", cmd.templates)
	} else if err != nil {
		return err
	}

	if cmd.out == "" {
		return fmt.Errorf("missing 'out' folder for the generated code")
	}

	if cmd.clean {
		log.Printf("deleting existing content from %v", cmd.out)
		if err := os.RemoveAll(cmd.out); err != nil {
			return err
		}
	}

	codegen := codegen.New(cmd.models, cmd.templates, cmd.out, cmd.debug)

	return codegen.Generate()
}
