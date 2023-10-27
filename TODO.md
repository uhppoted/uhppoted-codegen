# TODO

- [x] Lua bindings (cf. https://github.com/uhppoted/uhppoted-codegen/issues/10)
- [x] `set-door-passcodes` (cf. https://github.com/uhppoted/uhppoted/issues/40)
- [x] .ignore file (cf. https://github.com/uhppoted/uhppoted-codegen/issues/5)
- [ ] Replace Event pointer in GetStatusResponse with zero value (cf. https://github.com/uhppoted/uhppote-core/issues/18)
      - [x] Go
      - [x] Rust
      - [x] Python
      - [ ] Zig
      - [ ] PHP
      - [ ] Erlang
      - [ ] Lua
      - [ ] HTTP
      - [ ] CHANGELOG

- [ ] --delimiters
- [ ] clang-tidy


## zig
- (?) async/await timeouts
      - https://dev.to/hnakamur/experimenting-timeout-and-cancellation-with-zig-asyncawait-and-tigerbeetle-io-53o5
      - https://github.com/MasterQ32/zig-network/issues/30
      - https://kristoff.it/blog/zig-colorblind-async-await/

## Rust
- [ ] Move executor::block_on to higher level in protocol stack
      - https://stackoverflow.com/questions/67414951/rust-define-function-that-invokes-async-closure
      - https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d93c28509a1c67661f31ff820281d434
      - https://medium.com/swlh/demystifying-closures-futures-and-async-await-in-rust-part-1-closures-97e531e4dc50
      - https://users.rust-lang.org/t/how-can-i-return-a-generic-type/40536/5
      - https://blog.jcoglan.com/2019/04/22/generic-returns-in-rust/
      - https://blog.yoshuawuyts.com/why-async-rust/
      - https://stackoverflow.com/questions/67686409/how-to-pass-an-async-function-as-a-parameter-in-rust-pyo3

- (?) Unit tests

## Python
- (?) Unit tests
- (?) asyncio
      
## HTTP
- [ ] listen

## Lua
   - https://stackoverflow.com/questions/50506099/why-is-lua-not-respecting-my-sigint-signal
   - https://stackoverflow.com/questions/19145551/lua-socket-cannot-be-properly-stopped-by-ctrlc
   - https://www.lua.org/pil/9.4.html
   - https://stackoverflow.com/questions/12889361/lua-sockets-asynchronous-events
   - https://stackoverflow.com/questions/13219570/lualanes-and-luasockets
   - https://lunarmodules.github.io/copas/reference.html
   - https://lua-users.org/wiki/CoroutinesAsConnectionHandlers
   - https://github.com/cosock/cosock

## Other
   - [Val](https://github.com/val-lang/val-lang.github.io)
   - [Odin](https://odin-lang.org)
   - Elixir
   - Markdown
   - Latex
   - ASN.1
   - [Janet](https://janet-lang.org)
   - Haskell
   - q
   - F#
   - Clojure
   - [Austral](https://borretti.me/article/introducing-austral#status)
      - https://animaomnium.github.io/what-austral-proves/
   - [Pony](https://www.ponylang.io/discover/#why-pony)
   - https://github.com/dolmen-go/goproc
   - https://en.wikipedia.org/wiki/M4_(computer_language)
   - https://www.scryer.pl
   - Verse
   - [V](https://vlang.io)
   - [Mojo](https://www.modular.com/mojo)
   - [Factor](https://factorcode.orgs)
   - C#
   - [Fennel](https://fennel-lang.org)
   - [Fleng](http://www.call-with-current-continuation.org/fleng/fleng.html)

## Notes

1. (?) UDL
   - https://github.com/mozilla/uniffi-rs/blob/main/docs/diplomat-and-macros.md

2. Python UDP + asyncio
   - https://docs.python.org/3/howto/sockets.html
   - https://realpython.com/async-io-python/
   - https://stackoverflow.com/questions/24048126/timing-out-udp-socket-in-python
   - https://stackoverflow.com/questions/54689032/how-to-set-timeout-for-a-block-of-code-which-is-not-a-function-python3
   - https://stackoverflow.com/questions/35585935/start-async-function-without-importing-the-asyncio-package

3. [Apex](https://apexlang.io)
