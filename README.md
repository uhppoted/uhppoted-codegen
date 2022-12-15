![build](https://github.com/uhppoted/uhppoted-codegen/workflows/build/badge.svg)

# uhppoted-codegen

Code generation tool for creating native UHPPOTE controller interfaces in languages other than _Go_.

The initial [release](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.2) includes generated UHPPOTE bindings for:
- [Go](https://github.com/uhppoted/uhppoted-codegen/releases/download/v0.8.2/uhppoted-codegen_v0.8.2-go.tar.gz)
- [Rust](https://github.com/uhppoted/uhppoted-codegen/releases/download/v0.8.2/uhppoted-codegen_v0.8.2-rust.tar.gz)
- [Python](https://github.com/uhppoted/uhppoted-codegen/releases/download/v0.8.2/uhppoted-codegen_v0.8.2-python.tar.gz)
- [HTTP]()

## Raison d'être

Because not everybody uses _Go_ and although there is the [DLL](https://github.com/uhppoted/uhppoted-dll) for those
use cases it does require a DLL built for the target machine. And of course, sometimes a basic, uncomplicated
language binding that you can customize to your heart's content is what you really want anyway.

## Status

**IN DEVELOPMENT**

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
| v0.8.3    | Added ARM64 to release builds                                                             |
| v0.8.2    | Initial release                                                                           |

## Installation

Executables for all the supported operating systems are packaged in the [releases](https://github.com/uhppoted/uhppoted-codegen/releases):

The release tarballs contain the executables for all the operating systems - OS specific tarballs with all the _uhppoted_ components can be found in [uhpppoted](https://github.com/uhppoted/uhppoted/releases) releases.

Installation is straightforward - download the archive and extract it to a directory of your choice. 

`uhppoted-codegen help` will list the available commands and associated options (documented below).

### Building from source

Required tools:
- [Go 1.19+](https://go.dev)
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
| [uhppoted-lib](https://github.com/uhppoted/uhppoted-lib)                | Common function library              |


## uhppoted-codegen

Usage: ```uhppoted-codegen <command> <options>```

Supported commands:

- `help`
- `version`
- `generate`
- `export`

Defaults to `generate` if the command is not provided.

### `generate`

Generates a native UHPPOTE interface from the languages templates.

Command line:

` uhppoted-codegen generate [--debug] [--clean] --templates <folder> --out <folder>`

```
  --models <folder>     Folder with JSON model data.
  --templates <folder>  Folder containing the code generation templates for the target language.
  --out <folder>        Destination folder for the generated code.

  Options:

  --clean Erases 'out' folder before generating code from templates. Defaults to false
  --debug Displays verbose debugging information. Defaults to false

  Example:

  uhppoted-codegen generate --models bindings/.models --templates bindings/rust --out generated/rust
```

### `export`

Generates a _models.json_ file that represents the internal UHPPOTE models used to generate the functions,
requests and responses.

Command line:

` uhppoted-codegen export [--out <file>]`

```
  --out <file> File for models JSON. Defaults to models.json.

  Example:
  
  uhppoted-codegen export --out my-models.json
```

