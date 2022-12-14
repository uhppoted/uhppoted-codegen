# TODO

- [ ] https://github.com/dolmen-go/goproc
- [ ] https://en.wikipedia.org/wiki/M4_(computer_language)

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

- [x] github workflow
```
 Compiling uhppoted v0.1.0 (/home/runner/work/uhppoted-codegen/uhppoted-codegen/generated/rust/uhppoted)
warning: use of deprecated associated function `chrono::NaiveDate::from_ymd`: use `from_ymd_opt()` instead
   --> src/commands.rs:304:28
    |
304 |     let start = NaiveDate::from_ymd(2022, 1, 1);
    |                            ^^^^^^^^
    |
    = note: `#[warn(deprecated)]` on by default

warning: use of deprecated associated function `chrono::NaiveDate::from_ymd`: use `from_ymd_opt()` instead
   --> src/commands.rs:305:26
    |
305 |     let end = NaiveDate::from_ymd(2022, 12, 31);
    |
```

- (?) Unit tests

## Python
- (?) Unit tests
- (?) asyncio
      
## HTTP
- [ ] listen

## Other
   - Erlang
   - Elixir
   - Lua
   - Markdown
   - Latex
   - ASN.1
   - [Janet](https://janet-lang.org)
   - Haskell
   - q
   - [Austral](https://borretti.me/article/introducing-austral#status)
   - [Pony](https://www.ponylang.io/discover/#why-pony)

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
