# TODO

- [x] .templates folder for utility template files
      - [x] only if exists
- [ ] Generate to tempfile
- [ ] Test data
- (?) Move UHPPOTE models to JSON

- [ ] `set-ip`
      - [x] codegen
      - [x] Go
      - [ ] Rust
      - [ ] JS

## Go

- [x] --debug
- [x] --bind
- [x] --broadcast
- [x] dump if debug
- [x] Make dump look like Rust implementation
- [ ] Rework wait hack as read,readall,readnone
- [ ] Move UDP stuff to udp.go
- [ ] 'all' command
- (?) Use find a la Rust
- [ ] Restructure a lá Rust
- (?) Type constraint for send() return value
      - (?) Option

## Rust

- [ ] Rework wait hack as read,readall,readnone
- [ ] Report error and exit i.e. don't panic
- [ ] 'all' command
- [ ] Macro for timeout future
- [ ] uhppote::error with kind a lá std::io::error
      - [ ] replace all Box<dyn Error>
      - [ ] wrap std::io::error
- (?) https://docs.rs/futures/0.1.13/futures/future/fn.loop_fn.html

## Javascript

## Markdown

## ASN.1

