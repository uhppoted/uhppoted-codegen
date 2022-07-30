![build](https://github.com/uhppoted/uhppoted-codegen/workflows/build/badge.svg)

# uhppoted-codegen

Code generation tool for creating native UHPPOTE interfaces in languages other than Go

## Raison d'être

## Status

_IN DEVELOPMENT_

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
|           |                                                                                           |

## Installation

Executables for all the supported operating systems are packaged in the [releases](https://github.com/uhppoted/uhppoted-codegen/releases):

The release tarballs contain the executables for all the operating systems - OS specific tarballs with all the _uhppoted_ components can be found in [uhpppoted](https://github.com/uhppoted/uhppoted/releases) releases.

Installation is straightforward - download the archive and extract it to a directory of your choice. 

`uhppoted-codegen help` will list the available commands and associated options (documented below).

### Building from source

Required tools:
- [Go 1.18+](https://go.dev)
- make (optional but recommended)

To build using the included Makefile:

```
git clone https://github.com/uhppoted/uhppoted-codegen.git
cd uhppoted-codegen
make build
```

Without using `make`:
```
git clone https://github.com/uhppoted/uhppoted-codegen.git
cd uhppoted-codegen
go build -trimpath -o bin/ ./...
```

The above commands build the `uhppoted-codegen` executable to the `bin` directory.


#### Dependencies

| *Dependency*                                                            | *Description*                        |
| ----------------------------------------------------------------------- | -------------------------------------|
| [uhppote-core](https://github.com/uhppoted/uhppote-core)                | Device level API implementation      |


## uhppoted-codegen

Usage: ```uhppoted-codegen <command> <options>```

Supported commands:

- `help`
- `version`
- `generate`

Defaults to `generate` if the command it not provided.

### `generate`

Generates a native UHPPOTE interface from the languages templates.. 

Command line:

` uhppoted-codegen [--debug] --template <folder> --out <folder> [options]`

```
  --template <folder>  The folder containing the code generation templates for the target language.

  --out <folder> The folder for the generated code.

  --debug       Displays verbose debugging information

  Options:

```

