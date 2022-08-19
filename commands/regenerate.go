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

type Regenerate struct {
	out string
}

var REGENERATE = Regenerate{
	out: "models.json",
}

func (cmd *Regenerate) flags() *flag.FlagSet {
	flagset := flag.NewFlagSet("regen", flag.ExitOnError)

	flagset.StringVar(&cmd.out, "out", "models.json", "output folder for the generated models JSON")

	return flagset
}

func (cmd *Regenerate) Name() string {
	return "regen"
}

func (cmd *Regenerate) Description() string {
	return "Generates a models.json file from the internal UHPPOTE models"
}

func (cmd *Regenerate) Usage() string {
	return "uhppoted-codegen regen [--out <file>]"
}

func (cmd *Regenerate) Help() {
	fmt.Println()
	fmt.Println("  Usage: uhppoted-codegen regen [--out <file]")
	fmt.Println()
	fmt.Println("  Options:")
	fmt.Println()
	cmd.FlagSet().VisitAll(func(f *flag.Flag) {
		fmt.Printf("    --%-12s %s\n", f.Name, f.Usage)
	})
	fmt.Println()
}

func (cmd *Regenerate) FlagSet() *flag.FlagSet {
	return cmd.flags()
}

func (cmd *Regenerate) Execute(args ...interface{}) error {
	log.Printf("%s %s (PID %d)\n", APPLICATION, core.VERSION, os.Getpid())

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

	return nil
}
