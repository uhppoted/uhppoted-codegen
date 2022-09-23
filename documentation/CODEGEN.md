# Guide to using _uhppoted-codegen_

At its core, _uhppoted-codegen_ is just a templating engine that translates templatized documents into language specific
code - nothing special and there are dozens of similar implementations out there. However, it does ship with a set of 
[models](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/.models) for the UHPPOTE Wiegand controller and
a set of [example templates](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings) for generating a UHPPOTE
controller interface in:

- [Go](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/go)
- [Rust](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/rust)
- [Python](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/python)
- [Javascript](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/javascript)

The models are provided as JSON files so it's entirely possible to use an alternative templating engine. However, assuming
you've decided on using _uhppoted-codegen_, the remainder of this document outlines the process of creating a language
specific UHPPOTE binding.

## Quickstart

The distribution includes a _quickstart_ example which generates a (very) rough approximation to a real binding. It should
(hopefully) be reasonably obvious how everything fits together but for the curious there is more detail below and the Go,
Rust and Python bindings may also provide some insights.

To use the _quickstart_:

1. Clone the [quickstart]() folder to a language specific folder (e.g. _intercal_) under the bindings folder:
```
cp -r ./bindings/quickstart ./bindings/intercal
```

2. Rename the source files:
```
mv ./bindings/intercal/main.x                  ./bindings/intercal/main.i
mv ./bindings/intercal/commands.x              ./bindings/intercal/commands.i
mv ./bindings/intercal/uhppote.x               ./bindings/intercal/uhppote.i
mv ./bindings/intercal/encode.x                ./bindings/intercal/encode.i
mv ./bindings/intercal/decode.x                ./bindings/intercal/decode.i
mv ./bindings/intercal/udp.x                   ./bindings/intercal/udp.i
mv ./bindings/intercal/.templates/templates.x  ./bindings/intercal/.templates/templates.i

```

2. Create the language specific extensions in the _bindings/.models_ folder:
```
cp ./bindings/.models/quickstart.json ./bindings/.models/intercal.json

vim ./bindings/.models/intercal.json
...
{
    "intercal": {
        "types": {
            "uint8": "STRING",
            "uint16": "STRING",
...
```

3. Edit the language specific templates to reference the language specific model extensions:
```
vim ./bindings/intercal/.templates/templates.i
...
{{define "type"}}{{lookup "intercal.types" . "???"}}{{end}}
...
```

4. Run _uhppoted-codegen_ against the new binding:
```
uhppoted-codegen --models ./bindings/.models --templates ./bindings/intercal --out generated/intercal --clean
```

5. Adjust the code in the source templates to match the target language and rerun _uhppoted-codegen_ against the
updated templates until the generated code compiles and runs.
```
uhppoted-codegen --models ./bindings/.models --templates ./bindings/intercal --out generated/intercal --clean
intercal compile ./generated/intercal/*.i
intercal run     ./generated/intercal/main.i get-all-controllers
```

## Details

#### Go template language

The Go template language reference is [here](https://pkg.go.dev/text/template). Be warned, it's fairly dense and bits of
it are arcane enough that you will probably need to consult one of the many, many, many blog posts, articles and Stack
Overflow questions out there. 

#### Utility functions

- `CamelCase` formats a space delimited string as camel case, starting with an upper case letter
- `camelCase` formats a space delimited string as camel case, starting with a lower case letter
- `SnakeCase` formats a space delimited string as snake case, starting with an upper case letter
- `snakeCase` formats a space delimited string as camel case, starting with a lower case letter
- `kebabCase` formats a space delimited string as kebab case, starting with a lower case letter
- `lowercase` converts a string to all lowercase
- `uppercase` converts a string to all uppercase
- `trim` removes leading and trailing whitespace from a string
- `byte2hex` converts a byte array to the equivelent hexadecimal string
- `dump` converts a 64 byte message to a block of hexadecimal
- `lookup` gets a value from the model using a 'dotted' key (e.g. go.types.uint32)

### Models

The [models](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/.models) shipped with the example bindings
include:

- [models.json](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/models.json)
- [test-data.json](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/test-data.json)

as well as language specific support for:
- [Go](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/go.json)
- [Rust](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/rust.json)
- [Python](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/python.json)

#### [_models.json_](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/models.json)

_models.json_ provides a list of supported UHPPOTED functions, each function comprising:
- the function name
- the arguments to the function
- the request and request values required to encode the request
- the response and response fields required to decode the response

Technically, the arguments to the function match the values required to encode a request but are provided
separately to simplify the code generation templates.

e.g.:
```
{
  "model": {
    "functions": [
      {
        "name": "get controller",
        "args": [
          {
            "name": "device id",
            "type": "uint32"
          }
        ],
        "request": {
          "name": "get controller request",
          "msgtype": 148,
          "fields": [
            {
              "name": "device id",
              "type": "uint32",
              "offset": 4
            }
          ]
        },
        "response": {
          "name": "get controller response",
          "msgtype": 148,
          "fields": [
            {
              "name": "controller",
              "type": "uint32",
              "offset": 4
            },
            {
              "name": "ip address",
              "type": "IPv4",
              "offset": 8
            },
            ...
            {
              "name": "date",
              "type": "date",
              "offset": 28
            }
          ]
        }
      },
...
...
```

#### [_test-data.json_](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/test-data.json)

_test-data.json_ is a set of test cases intended for generating unit tests to verify a language binding. Each test case
includes known request/response values and the corresponding encoded request and response messages, 

e.g.
```
{
  "testdata": {
    "tests": [
      ...
      {
        "name": "set time",
        "request": {
          "name": "set time request",
          "values": [
            {
              "name": "controller",
              "type": "uint32",
              "value": "405419896"
            },
            {
              "name": "datetime",
              "type": "datetime",
              "value": "2022-08-23 09:49:03"
            }
          ],
          "message": "FzAAAHg3KhggIggjCUkDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="
        },
        "response": {
          "name": "set time response",
          "values": [
            {
              "name": "controller",
              "type": "uint32",
              "value": "405419896"
            },
            {
              "name": "datetime",
              "type": "datetime",
              "value": "2022-08-23 09:49:03"
            }
          ],
          "message": "FzAAAHg3KhggIggjCUkDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="
        }
      },
      ...
```

For an example of using _test-data.json_ look at t=he _Go_ language binding which includes generated unit tests for encoding requests and decoding responses.

#### [_go.json_](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/go.json)

The _go.json_ models include any _Go_ specific information - specifically the type conversions from the _models_ types to
valid _Go_ types:
```
{
    "go": {
        "types": {
            "uint8": "uint8",
            "uint16": "uint16",
            "uint32": "uint32",
            "bool": "bool",
            "IPv4": "netip.Addr",
            ...
```

The _Go_ types are used in the _Go_ bindings common [templates](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/go/.templates):
```
{{define "type"}}{{lookup "go.types" . "???"}}{{end}}
```

#### [_rust.json_](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/rust.json)

The _rust.json_ models include any _Rust_ specific information - specifically the type conversions from the _models_ types to
valid _Rust_ types:
```
{
    "rust": {
        "types": {
            "uint8": "u8",
            "uint16": "u16",
            "uint32": "u32",
            "bool": "bool",
            "IPv4": "Ipv4Addr",
            ...
```

The _Rust_ types are used in the _Rust_ bindings common [templates](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/rust/.templates):
```
{{define "type"}}{{lookup "rust.types" . "???"}}{{end}}
```

#### [_python.json_](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/rust.json)

The _python.json_ models include any _Python_ specific information - specifically the type conversions from the _models_ types to
valid _Python_ types:
```
    "python": {
        "types": {
            "uint8": "int",
            "uint16": "int",
            "uint32": "int",
            "bool": "bool",
            "IPv4": "IPv4Address",
            "MAC": "str",
            ...
```

The _Python_ types are used in the _Python_ bindings common [templates](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/python/.templates):
```
{{define "type"}}{{lookup "python.types" . "???"}}{{end}}
```

### Interface structure

The suggested structure for a language binding comprises the following components:

| Component | [Go](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/go) | [Rust](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/rust) | [Python](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/python) | [Javascript](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/http) |
|---------------------|---------------|---------------|---------------|---------------|
| API                 | _main.go_     | _main.rs_     | _main.py_     | _main.js_     |
| Command executor    | _commands.go_ | _commands.rs_ | _commands.py_ | _commands.js_ |
| UHPPOTE driver      | _uhppote.go_  | _uhppote.rs_  | _uhppote.py_  | _uhppote.js_  |
| Request encoder     | _encode.go_   | _encode.rs_   | _encode.py_   | _encode.js_   |
| Response decoder    | _decode.go_   | _decode.rs_   | _decode.py_   | _decode.js_   |
| UDP driver          | _udp.go_      | _udp.rs_      | _udp.py_      | _udp.js_      |


#### API

The API component provides the externally visible programming interface for the rest of the application or library. In the
Go, Rust and Python examples the API implements a CLI application, while the Javascript example implements an API for
[_uhppoted-tunnel::http_](https://github.com/uhppoted/uhppoted-tunnel/tree/master/examples/html).

Functionally, the API component is responsible for:

- getting the parameters for each command
- submitting the command and parameters to the command executor
- processing the response (e.g. displaying the result)
- error handling

In all the examples, the API component is language and application specific and is hand-coded, with the common functionality
delegated to the _commands_ component.

#### Commands

The _commands_ component is functionally responsible for dispatching an API request to the UHPPOTE driver and returning the
result or error. 

In all the examples, the _commands_ component is implemented as a map of commands to the associated function that implements
the specific details of translating the API function to a UHPPOTE function call. The supported functions include:

- `get-all-controllers`
- `get-controller`
- `set-ip`
- `get-time`
- `set-time`
- `get-listener`
- `set-listener`
- `get-door-control`
- `set-door-control`
- `get-status`
- `open-door`
- `get-cards`
- `get-card`
- `get-card-by-index`
- `put-card`
- `delete-card`
- `delete-all-cards`
- `get-event`
- `get-event-index`
- `set-event-index`
- `record-special-events`
- `get-time-profile`
- `set-time-profile`
- `delete-all-time-profiles`
- `add-task`
- `refresh-tasklist`
- `clear-tasklist`
- `listen`

As with the API component, the _commands_ component is typically language and application specific and is probably
easiest to code by hand.

#### UHPPOTE

Finally we get to code that can actually be generated ... the _UHPPOTE_ component is functionally responsible for:

- encoding the command values into a UHPPOTE command message
- dispatching the command to the controller
- decoding the reply from the controller
- returning the structured response

There are two functions that it is easier to code by hand:

- `get-all-controllers`
- `listen`

The remaining commands can be generated from the information in the _models.json_ file. Typically the structure of a function
looks something like:
```
function  <name> (<list of arguments>) {
    request = encode<name>(<list of argmuments>)
    reply,error = UDP.send(request)
    
    if error {
        return error
    } else {
        return decode<name>(reply)
    }
} 
```

In the template language, that gets encoded as (for example):
```
func {{CamelCase .name}}({{template "args" .args}}) (*{{CamelCase .response.name}},error) {
    request,err := {{CamelCase .request.name}}({{template "params" .args}})
    if err != nil {
        return nil,err
    }

    if reply,err := send(request); err != nil {
        return nil,err
    } else if response,err := {{camelCase .response.name}}(reply); err != nil {
        return nil, err
    } else if response != nil {
        return response, nil
    }

    return nil, nil
}
```

#### Request encoder

The request encoder comprises:

- a set of generated functions to encode requests 
- a set of hand coded language specific routines to pack the language specific types into a byte array

The automatically generated functions are generated from the _models_ requests list and encode each request
into a 64 byte array, e.g.:
```
{{range .model.requests}}
{{- template "request" . -}}
{{end}}

{{define "request"}}
function {{.name}}({{template "args" .fields}}) []byte {
    packet = byte[64]

    packet[0] = 0x17
    packet[1] = {{byte2hex .msgtype}}
    {{range .fields -}}
    pack{{CamelCase .type}}({{.name}}, packet, {{.offset}})
    {{end}}
    
    return packet, nil
}
{{end}}
...
```

The _request_ function template references a set of hand coded routines to encode each data type into the 
byte array, e.g.:
```
function packUint8(v uint8, packet []byte, offset uint8) {
    packet[offset] = v
}

function packUint16(v uint16, packet []byte, offset uint8) {
    Uint16.LittleEndian(packet[offset:offset+2], v)
}
...
```

The example templates expect the following _pack_ functions to be supplied:

- `packUint8`
- `packUint16`
- `packUint32`
- `packIPv4`
- `packDate`
- `packDatetime`
- `packHHmm`
- `packBool`

#### Response decoder

The response decoder comprises:

- a generated _container_ for each response
- a generated _container_ for an event 
- a set of generated functions to decode each response
- a generated function to decode an event message
- a set of hand coded language specific routines to unpack the byte array into language specific types

The automatically generated functions are generated from the _models_ responses list and decode each response
from a 64 byte array into whatever representation makes sense in the language/application (typically a `struct`
or an `object`), e.g.:
```
{{range .model.response}}
{{- template "response" . -}}
{{end}}

{{with .model.event}}
{{- template "event" . -}}
{{end}}

{{range .model.response}}
{{- template "decode" . -}}
{{end}}

{{with .model.event}}
{{- template "decode" . -}}
{{end}}

{{define "response"}}
struct {{.name}} { {{range .fields}}
    {{.name}} {{template "type" .type}}
}
{{end}}

{{define "event"}}
struct {{.name}} { 
    {{range .fields}}
    {{.name}} {{template "type" .type}}
    {{end}}
}
{{end}}

{{define "decode"}}
function {{.name}}(packet []byte) {
    if len(packet) != 64 {
        throw error("invalid reply packet length (%v)", len(packet))
    }

    if packet[1] != {{byte2hex .msgtype}} {
        throw error("invalid reply function code (%02x)", packet[1])
    }

    response = {{.name}}{
    {{range .fields}}
      v.{{.name}} = unpack{{CamelCase .type}}(packet, {{.offset}}),
    {{end}}
    }
    
    return response
}
{{end}}
...
```

The _response_ function template references a set of hand coded routines to decode each data type from the
byte array, e.g.:
```
function unpackUint8(packet []byte, offset uint8) {
    return packet[offset]
}

function unpackUint16(packet []byte, offset uint8) {
    return Uint16.LittleEndian(packet[offset:offset+2])
}

...
```

The example templates expect the following _unpack_ functions to be supplied:

- `unpackUint8`
- `unpackUint16`
- `unpackUint32`
- `unpackBool`
- `unpackIPv4`
- `unpackMAC`
- `unpackVersion`
- `unpackDate`
- `unpackShortDate`
- `unpackOptionalDate`
- `unpackDatetime`
- `unpackOptionalDatetime`
- `unpackTime`
- `unpackHHmm`

#### UDP driver

The UDP component is a hand-coded language specific implementation of the network functions required to send and receive
the 64 byte request/response message to/from the controller. Typically the UDP component implements the following
functions:

- _broadcast_
- _send_
- _listen_

In most implementations, UDP will bind to the `ALL` address 0.0.0.0:0 and send packets to port 60000 on the broadcast
address (255.255.255.255:60000). The examples provide functions to set the bind, broadcast and listen addresses.

##### _broadcast_

_broadcast_ sends a request to all controllers (i.e. controller 0) and collects the responses. It is typically only 
used for the _get-all-controllers- command but it is possible to use it for other commands e.g. _get-time_.

In the examples _broadcast_ simply waits a hard-coded 5 seconds for all responses, but alternative implemenations
are possible.

##### _send_

_sends_ sends a request directed to a single controller and waits for the response, returning a timeout error
if no response is received within e.g. 5 seconds.

##### _listen_

_listen_ binds to a local address and port and receives real-time'ish events from a controller, dispatching them to
a handler function for decoding and processing.

### Implementation Notes

#### UHPPOTE driver

#### Request encoder

#### Response decoder

#### UDP driver
