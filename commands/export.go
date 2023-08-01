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
	lib "github.com/uhppoted/uhppoted-lib/os"
)

type Export struct {
	models   string
	testdata string
}

var EXPORT = Export{
	models:   "models.json",
	testdata: "test-data.json",
}

func (cmd *Export) flags() *flag.FlagSet {
	flagset := flag.NewFlagSet("regen", flag.ExitOnError)

	flagset.StringVar(&cmd.models, "models", "models.json", "output file for the generated models JSON")
	flagset.StringVar(&cmd.testdata, "tests", "", "(optional) output file for the generated test data JSON")

	return flagset
}

func (cmd *Export) Name() string {
	return "export"
}

func (cmd *Export) Description() string {
	return "Generates a models.json file from the internal UHPPOTE models"
}

func (cmd *Export) Usage() string {
	return "uhppoted-codegen export [--models <file>] [--testdata <file>]"
}

func (cmd *Export) Help() {
	fmt.Println()
	fmt.Println("  Usage: uhppoted-codegen export [--models <file] [--testdata <file]")
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

	if cmd.models == "" {
		return fmt.Errorf("missing 'models' file for the generated models JSON")
	}

	if err := cmd.genModels(); err != nil {
		return err
	}

	if cmd.testdata != "" {
		if err := cmd.genTestData(); err != nil {
			return err
		}
	}

	return nil
}

func (cmd *Export) genModels() error {
	uhppote := struct {
		Model model.Model `json:"model"`
	}{
		Model: model.Model{
			Functions: model.Functions,
			Requests:  model.Requests,
			Responses: model.Responses,
			Event:     model.Event,
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
	dest := cmd.models
	if err := os.MkdirAll(filepath.Dir(dest), 0777); err != nil {
		return err
	} else if err := lib.Rename(f.Name(), dest); err != nil {
		return err
	}

	log.Printf("%s exported models to %v", APPLICATION, dest)

	return nil
}

func (cmd *Export) genTestData() error {
	testdata := struct {
		TestData model.TestData `json:"testdata"`
	}{
		TestData: model.TestData{
			Tests: model.Tests,
		},
	}

	bytes, err := json.MarshalIndent(testdata, "", "  ")
	if err != nil {
		return err
	}

	// ... write to tempfile
	f, err := os.CreateTemp("", "uhppote-test-data-*.json")
	if err != nil {
		return err
	} else {
		defer os.Remove(f.Name())
	}

	if err := os.WriteFile(f.Name(), bytes, 0660); err != nil {
		return err
	}

	// ... copy to destination file
	dest := cmd.testdata
	if err := os.MkdirAll(filepath.Dir(dest), 0777); err != nil {
		return err
	} else if err := lib.Rename(f.Name(), dest); err != nil {
		return err
	}

	log.Printf("%s exported test data to %v", APPLICATION, dest)

	return nil
}
