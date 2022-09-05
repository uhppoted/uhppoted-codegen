# TODO

- [ ] Rework templates with {{- and -}}
      - [x] Go
      - [ ] Rust
      - [ ] Python
      - [ ] HTTP

- [ ] Test data
      - [x] messages
      - [ ] request values
      - [ ] response values

- [ ] Code generation HOWTO
- [ ] Package for release
- [ ] Add code generation to github workflow

- [ ] listen
      - [x] Go
      - [ ] Rust
            - [ ] CTRL-C
            - (?) Rework as stream
            - https://stackoverflow.com/questions/71524673/how-to-stop-retry-loop-with-async
            - https://docs.rs/futures/0.1.2/futures/fn.oneshot.html
      - [x] Python
      - [x] Remove from 'all'

## Go
- [ ] Optional time profile dates
- [ ] Handle missing/deleted card
- [ ] Handle missing time profile
- [ ] Unit tests
- (?) Type constraint/generic/variant for send() return value

## Rust
- [ ] Optional time profile dates
- [ ] Handle missing time profile
- [ ] Handle missing/deleted card
- (?) Use `Either` for result of read functions
- (?) Replace executor::block with await
      - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d93c28509a1c67661f31ff820281d434

## Python
- [ ] Optional time profile dates
- [ ] Handle missing time profile
- [ ] Handle missing/deleted card
- [ ] asyncio
      - https://docs.python.org/3/howto/sockets.html
      - https://stackoverflow.com/questions/24048126/timing-out-udp-socket-in-python
      - https://stackoverflow.com/questions/54689032/how-to-set-timeout-for-a-block-of-code-which-is-not-a-function-python3
      - https://stackoverflow.com/questions/35585935/start-async-function-without-importing-the-asyncio-package
      - https://realpython.com/async-io-python/
      - https://stackoverflow.com/questions/54689032/how-to-set-timeout-for-a-block-of-code-which-is-not-a-function-python3

## HTTP
- [x] Implement remaining commands
- [ ] `listen`

## Erlang

## Elixir

## Lua

## Markdown

## Latex

## ASN.1

