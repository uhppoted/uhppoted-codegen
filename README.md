![build](https://github.com/uhppoted/uhppoted-codegen/workflows/build/badge.svg)

# uhppoted-codegen

Code generation tool for creating native UHPPOTE controller interfaces in multiple languages:
- Go
- Rust
- Python
- Zig
- PHP
- HTML + Javascript

## Raison d'être

Because not everybody uses _Go_ and a sometimes a basic, uncomplicated language binding as a place to start and
that you can customize to your heart's content is what you really want.

The codegen'd bindings are an alternative to the [DLL](https://github.com/uhppoted/uhppoted-dll) which also supports
development in languages other than Go. However, because of the way the cgo compiler compiles shared libraries it does
require that a DLL be built specifically for the target machine which can make application distribution more complicated
than it should be.

## Releases

| *Version* | *Description*                                                                             |
| --------- | ----------------------------------------------------------------------------------------- |
| v0.8.4    | Added Zig to included language bindings and added `set-pc-control` to generated functions |
| v0.8.3    | Added ARM64 to release builds                                                             |
| v0.8.2    | Initial release                                                                           |

## Installation

Executables for all the supported operating systems are packaged in the [releases](https://github.com/uhppoted/uhppoted-codegen/releases):

The release tarballs contain the executables for all the operating systems - OS specific tarballs with all the _uhppoted_ components can be found in [uhpppoted](https://github.com/uhppoted/uhppoted/releases) releases.

Installation is straightforward - download the archive and extract it to a directory of your choice. 

`uhppoted-codegen help` will list the available commands and associated options (documented below).

### Building from source

Required tools:
- [Go 1.20+](https://go.dev)
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
|                                                                         |                                      |


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

## Notes

### `put-card`

The UHPPOTE access controller has a weird behaviour around the PIN field. According to the SDK 
documentation, valid PINs are in the range 0 to 999999. However the controller will accept a 
PIN number out of that range and only keep the lower 7 nibbles of the 32-bit unsigned value.
e.g:

| PIN     | Hex value | Stored as (hex) | Retrieved as (hex) | Retrieved as (decimal) |
|---------|-----------|-----------------|--------------------|------------------------|
| 0       | 0x000000  | 0x000000        | 0x000000           | 0                      |
| 999999  | 0x0f423f  | 0x0f423f        | 0x0f423f           | 999999                 |
| 1000000 | 0x0f4240  | 0x000000        | 0x000000           | 0                      |
| 1000001 | 0x0f4241  | 0x000000        | 0x000000           | 0                      |
| 1048576 | 0x100000  | 0x000000        | 0x000000           | 0                      |
| 1048577 | 0x100001  | 0x000000        | 0x000001           | 1                      |
| 1999999 | 0x1E847F  | 0x0E847F        | 0x000001           | 951423                 |

Please be aware that (unlike the _uhppote-core_ `put-card` implementation) the codegen bindings
do allow out of range PINs.


