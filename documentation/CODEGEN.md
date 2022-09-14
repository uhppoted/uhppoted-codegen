# Basic Guide to Using uhppoted-codegen

At it's core, _uhppoted-codegen_ is just a templating engine that translates templatized documents into language specific
code - nothing special and there are dozens of similar implementations out there. However, it does ship with a set of 
[models](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/.models) for the UHPPOTE Wiegand controller and
a set of [example templates](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings for generating a UHPPOTE
controller interface in:

- Go
- Rust
- Python
- Javascript

The models are provided as JSON files so it's entirely possible to use an alternative templating engine. 

Assuming you've decided on using _uhppoted-codegen_, the remainder of this document outlines the process of
creating a language specific UHPPOTE binding.

## Preliminary Notes

### Go template language

### Template functions

- CamelCase
- camelCase
- SnakeCase
- snakeCase
- kebabCase
- lowercase
- uppercase
- trim
- byte2hex
- dump
- lookup

## Models

The [models](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/.models) shipped with the example bindings
include:

- [models.json](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/models.json)
- [test-data.json](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/test-data.json)

as well as the models for language specific support:
- [Go](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/go.json)
- [Rust](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/rust.json)
- [Python](https://github.com/uhppoted/uhppoted-codegen/blob/main/bindings/.models/python.json)

### _models.json_

### _test-data.json_

### _go.json_

### _rust.json_

### _python.json_

## Interface structure

The suggested structure for a language binding comprises the following components:

| Component | [Go](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/go) | [Rust](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/rust) | [Python](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/python) | [Javascript](https://github.com/uhppoted/uhppoted-codegen/tree/main/bindings/http) |
|---------------------|---------------|---------------|---------------|---------------|
| API                 | _main.go_     | _main.rs_     | _main.py_     | _main.js_     |
| Command executor    | _commands.go_ | _commands.rs_ | _commands.py_ | _commands.js_ |
| UHPPOTE driver      | _uhppote.go_  | _uhppote.rs_  | _uhppote.py_  | _uhppote.js_  |
| Request encoder     | _encode.go_   | _encode.rs_   | _encode.py_   | _encode.js_   |
| Response decoder    | _decode.go_   | _decode.rs_   | _decode.py_   | _decode.js_   |
| UDP driver          | _udp.go_      | _udp.rs_      | _udp.py_      | _udp.js_      |


### API

The API component provides the externally visible programming interface for the rest of the application or library. In the
Go, Rust and Python examples the API implements a CLI application, while the Javascript example implements an API for [_uhppoted-tunnel::http_](https://github.com/uhppoted/uhppoted-tunnel/tree/master/examples/html).

Functionally, the API component is responsible for:

- getting the parameters for each command
- submitting the command + parameters to the command executor
- processing the response (e.g. displaying the result)
- error handling

In all the examples, the API component is language and application specific and is hand-coded, with the common functionality
delegated to the command component.

### Commands

The _commands_ component functionally responsible for dispatching an API request to the UHPPOTE driver and returning the
result (or error). 

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

As with the API component, the _commands_ component is typically language and application specific and is probably easiest to code by hand.

### UHPPOTE

Finally we get to code that can actually be generated ...

The _UHPPOTE_ component is functionally responsible for:

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

In the Go template language, that gets encoded as (for example):
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

### Request encoder

### Response decoder

### UDP driver


## Implementation

### UHPPOTE driver

### Request encoder

### Response decoder

### UDP driver
