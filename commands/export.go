package commands

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"
	"path/filepath"

	core "github.com/uhppoted/uhppote-core/uhppote"

	"github.com/uhppoted/uhppoted-codegen/model"
)

type Export struct {
	out string
}

var EXPORT = Export{
	out: "models.json",
}

func (cmd *Export) flags() *flag.FlagSet {
	flagset := flag.NewFlagSet("regen", flag.ExitOnError)

	flagset.StringVar(&cmd.out, "out", "models.json", "output folder for the generated models JSON")

	return flagset
}

func (cmd *Export) Name() string {
	return "export"
}

func (cmd *Export) Description() string {
	return "Generates a models.json file from the internal UHPPOTE models"
}

func (cmd *Export) Usage() string {
	return "uhppoted-codegen export [--out <file>]"
}

func (cmd *Export) Help() {
	fmt.Println()
	fmt.Println("  Usage: uhppoted-codegen export [--out <file]")
	fmt.Println()
	fmt.Println("  Options:")
	fmt.Println()
	cmd.FlagSet().VisitAll(func(f *flag.Flag) {
		fmt.Printf("    --%-12s %s\n", f.Name, f.Usage)
	})
	fmt.Println()
}

func (cmd *Export) FlagSet() *flag.FlagSet {
	return cmd.flags()
}

func (cmd *Export) Execute(args ...interface{}) error {
	log.Printf("%s %s (PID %d)", APPLICATION, core.VERSION, os.Getpid())

	if cmd.out == "" {
		return fmt.Errorf("missing 'out' folder for the generated models.json")
	}

	uhppote := struct {
		Model model.Model `json:"model"`
	}{
		Model: model.Model{
			Functions: model.Functions,
			Requests:  model.Requests,
			Responses: model.Responses,
		},
	}

	bytes, err := json.MarshalIndent(uhppote, "", "  ")
	if err != nil {
		return err
	}

	// ... write to tempfile
	f, err := os.CreateTemp("", "uhppote-models-*.json")
	if err != nil {
		return err
	} else {
		defer os.Remove(f.Name())
	}

	if err := os.WriteFile(f.Name(), bytes, 0660); err != nil {
		return err
	}

	// ... copy to destination file
	dest := cmd.out
	if err := os.MkdirAll(filepath.Dir(dest), 0777); err != nil {
		return err
	} else if err := os.Rename(f.Name(), dest); err != nil {
		return err
	}

	log.Printf("%s exported models to %v", APPLICATION, dest)

	return nil
}
