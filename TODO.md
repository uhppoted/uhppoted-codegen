# TODO

- [x] .ignore file (cf. https://github.com/uhppoted/uhppoted-codegen/issues/5)
      - [x] README
      - [x] CHANGELOG
      - [x] globbing
```
      ERROR: error parsing src/.encode.lua.swp (template: codegen:12: unexpected {{end}})
```

- [ ] Lua bindings (cf. https://github.com/uhppoted/uhppoted-codegen/issues/10)
      - [x] generate bindings
      - [x] set-bind-address
      - [x] set-broadcast-address
      - [x] set-listen-address
      - [x] commands
             - [x] get-devices
             - [x] get-device
             - [x] set-ip
             - [x] get-time
             - [x] set-time
             - [x] get-status
             - [x] get-status-no-event
             - [x] get-listener
             - [x] set-listener
             - [x] get-door-control
             - [x] set-door-control
             - [x] open-door
             - [x] get-cards
             - [x] get-card
             - [x] get-card-by-index
             - [x] put-card
             - [x] delete-card
             - [x] delete-all-cards
             - [x] get-event
             - [x] get-event-index
             - [x] set-event-index
             - [x] record-special-events
             - [x] get-time-profile
             - [x] set-time-profile
             - [x] delete-all-time-profiles
             - [x] add-task
             - [x] refresh-tasklist
             - [x] clear-tasklist
             - [x] set-pc-control
             - [x] set-interlock
             - [x] activate-keypads
             - [x] set-door-passcodes
             - [x] listen
      - [x] github workflow + build status
      - [x] stylua
      - [x] documentation
      - [x] dist
      - [x] CHANGELOG
      - [x] README
      - [ ] listen:interrupt
            - https://www.lua.org/pil/9.4.html
            - https://stackoverflow.com/questions/12889361/lua-sockets-asynchronous-events
            - https://jariou.gitbooks.io/comprehensive-lua/content/calling_c_modules_from_lua/networking_with_coroutines_example_using_luasocket.html
            - https://stackoverflow.com/questions/13219570/lualanes-and-luasockets
            - https://lunarmodules.github.io/copas/reference.html
            - https://lua-users.org/wiki/CoroutinesAsConnectionHandlers
            - https://github.com/cosock/cosock
            - https://stackoverflow.com/questions/50506099/why-is-lua-not-respecting-my-sigint-signal
            - https://stackoverflow.com/questions/19145551/lua-socket-cannot-be-properly-stopped-by-ctrlc
            - https://www.lua.org/pil/9.4.html
            - (?) non-blocking socket + select


- [x] `set-door-passcodes` (cf. https://github.com/uhppoted/uhppoted/issues/40)

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

## Other
   - Lua
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
