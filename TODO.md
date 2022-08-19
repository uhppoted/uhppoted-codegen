# TODO

- [ ] Move UHPPOTE models to JSON
      - [x] `regen` command
      - [ ] Use models.json

- [ ] Test data

- [x] get-listener
      - [x] codegen
      - [x] Go
      - [x] Rust

## Go
- [ ] BCD error handling
- (?) JSON tags for requests/responses
- (?) Type constraint/variant for send() return value

## Rust
- [x] uhppote::error with kind a lá std::io::error
      - [x] replace all Box<dyn Error>
- [x] Rework futures into something vaguely sane
- [ ] Wrap udp::read asyncs into something HOF'able
      - (?) move executor::block into read
- [ ] BCD error handling


## Javascript

## Python

## Erlang

## Elixir

## Lua

## Markdown

## Latex

## ASN.1

