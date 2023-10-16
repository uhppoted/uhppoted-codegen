DIST    ?= development
DEBUG   ?= --debug
CMD      = ./bin/uhppoted-codegen
COMMAND ?= get-all-controllers

MODELS = bindings/.models
QUICKSTART = bindings/quickstart
GO     = bindings/go
RUST   = bindings/rust
PYTHON = bindings/python
HTTP   = bindings/http
ZIG    = bindings/zig
PHP    = bindings/php
ERLANG = bindings/erlang
LUA    = bindings/lua

GOBIN   = ./generated/go/bin/uhppoted
RUSTBIN = ./generated/rust/uhppoted/target/debug/uhppoted
PYBIN   = python3 ./generated/python/main.py
ZIGBIN  = ./generated/zig/zig-out/bin/uhppoted
PHPBIN  = php ./generated/php/uhppoted.php
ERLBIN  = ./generated/erlang/_build/default/bin/cli
LUABIN  = cd ./generated/lua && lua main.lua

.DEFAULT_GOAL := lua-debug
.PHONY: update
.PHONY: update-release

all: test      \
     benchmark \
     coverage

clean:
	go clean
	rm -rf bin

update:
	go get -u github.com/uhppoted/uhppote-core@master
	go get -u github.com/uhppoted/uhppoted-lib@master
	go mod tidy

update-release:
	go get -u github.com/uhppoted/uhppote-core
	go get -u github.com/uhppoted/uhppoted-lib
	go mod tidy

format: 
	go fmt ./cmd/...
	go fmt ./commands/...
	go fmt ./codegen/...
	go fmt ./model/...

build: format
	go build -trimpath -o bin/ ./...

test: build
	go test ./...

vet: 
	go vet ./...

lint: 
	env GOOS=darwin  GOARCH=amd64 staticcheck ./...
	env GOOS=linux   GOARCH=amd64 staticcheck ./...
	env GOOS=windows GOARCH=amd64 staticcheck ./...

vuln:
	govulncheck ./...

benchmark: build
	go test -count 5 -bench=.  ./system/events

coverage: build
	go test -cover ./...

regen: 
	$(CMD) export --models bindings/.models/models.json --tests bindings/.models/test-data.json

build-all: test vet lint go rust python zig php erlang http
	mkdir -p dist/$(DIST)/windows
	mkdir -p dist/$(DIST)/darwin
	mkdir -p dist/$(DIST)/linux
	mkdir -p dist/$(DIST)/arm
	mkdir -p dist/$(DIST)/arm7
	env GOOS=linux   GOARCH=amd64         GOWORK=off go build -trimpath -o dist/$(DIST)/linux   ./...
	env GOOS=linux   GOARCH=arm64         GOWORK=off go build -trimpath -o dist/$(DIST)/arm     ./...
	env GOOS=linux   GOARCH=arm   GOARM=7 GOWORK=off go build -trimpath -o dist/$(DIST)/arm7    ./...
	env GOOS=darwin  GOARCH=amd64         GOWORK=off go build -trimpath -o dist/$(DIST)/darwin  ./...
	env GOOS=windows GOARCH=amd64         GOWORK=off go build -trimpath -o dist/$(DIST)/windows ./...

release: update-release build regen build-all 
	tar --directory=dist      --exclude=".DS_Store" -cvzf dist/$(DIST).tar.gz $(DIST)
	tar --directory=.         --exclude=".DS_Store" -cvzf dist/$(DIST)-bindings.tar.gz ./bindings
	tar --directory=generated --exclude=".DS_Store" --exclude="go/bin"                                -cvzf dist/$(DIST)-go.tar.gz       go
	tar --directory=generated --exclude=".DS_Store" --exclude="rust/uhppoted/target"                  -cvzf dist/$(DIST)-rust.tar.gz     rust
	tar --directory=generated --exclude=".DS_Store" --exclude="python/__pycache__"                    -cvzf dist/$(DIST)-python.tar.gz   python
	tar --directory=generated --exclude=".DS_Store" --exclude="zig/zig-cache" --exclude="zig/zig-out" -cvzf dist/$(DIST)-zig.tar.gz      zig
	tar --directory=generated --exclude=".DS_Store" --exclude="php/.php-cs-fixer.cache"               -cvzf dist/$(DIST)-php.tar.gz      php
	tar --directory=generated --exclude=".DS_Store" --exclude="erlang/*.beam"                         -cvzf dist/$(DIST)-erlang.tar.gz   erlang
	tar --directory=generated --exclude=".DS_Store"                                                   -cvzf dist/$(DIST)-lua.tar.gz      lua

publish: release
	echo "Releasing version $(VERSION)"
	gh release create "$(VERSION)" \
	"./dist/uhppoted-codegen_$(VERSION).tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-bindings.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-go.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-python.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-rust.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-zig.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-php.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-erlang.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-lua.tar.gz" \
	--draft --prerelease --title "$(VERSION)-beta" --notes-file release-notes.md

debug: erlang
	cd generated/erlang && ./_build/default/bin/cli --debug \
	                                                --bind 192.168.1.100:0 \
	                                                --broadcast 192.168.1.255:60000 \
	                                                --listen 0.0.0.0:60001 \
	                                                set-ip

godoc:
	godoc -http=:80	-index_interval=60s

version: build
	$(CMD) version

help: build
	$(CMD) help
	$(CMD) help commands
	$(CMD) help version
	$(CMD) help help

export: build
	$(CMD) export --models runtime/models.json --tests runtime/test-data.json

quickstart: build
	$(CMD) --models $(MODELS) --templates $(QUICKSTART) --out generated/quickstart --clean

go: build regen
	$(CMD) --models $(MODELS) --templates $(GO) --out generated/go --clean
	cd generated/go && go fmt ./... && go mod tidy && go build -o ./bin/ ./...

go-test: go
	cd generated/go && go test -v ./uhppote

go-debug: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 set-door-passcodes

go-usage: regen build
	$(GOBIN)

go-cmd: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 $(COMMAND)

go-all: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

go-listen: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 listen

rust: build regen 
	$(CMD) --models $(MODELS) --templates $(RUST) --out generated/rust
	cd generated/rust/uhppoted && cargo fmt && cargo build

rust-debug: rust
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 set-door-passcodes"

rust-usage: rust
	$(RUSTBIN)

rust-cmd: rust
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 $(COMMAND)"

rust-all: rust
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

rust-listen: rust
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 listen"

python: build regen 
	$(CMD) --models $(MODELS) --templates $(PYTHON) --out generated/python
	cd generated/python && yapf -ri .
	chmod +x generated/python/main.py

python-debug: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 set-door-passcodes

python-usage: python
	$(PYBIN)

python-cmd: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 $(COMMAND)

python-all: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255 --listen 192.168.1.100:60001 all

python-listen: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 listen

zig: build regen
	$(CMD) --models $(MODELS) --templates $(ZIG) --out generated/zig --clean
	cd generated/zig && zig fmt src/* && zig build

zig-debug: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 set-door-passcodes

zig-usage: zig
	$(ZIGBIN) 

zig-cmd: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 $(COMMAND)

zig-all: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

zig-listen: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 listen

php: build regen
	$(CMD) --models $(MODELS) --templates $(PHP) --out generated/php --clean
	cd generated/php && php-cs-fixer fix .

php-debug: php
	$(PHPBIN) --debug --timeout=1 --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 set-door-passcodes

php-usage: php
	$(PHPBIN) 

php-cmd: php
	$(PHPBIN) --debug --timeout=1 --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 $(COMMAND)

php-all: php
	$(PHPBIN) --debug --timeout=1 --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 all

php-listen: php
	$(PHPBIN) --debug --timeout=1 --bind=192.168.1.100 --broadcast=192.168.1.255:60000 --listen=192.168.1.100:60001 listen

erlang: build regen
	# cd generated/erlang && dialyzer --src *.erl
	# cd generated/erlang && erl -compile main commands uhppoted udp encoder decoder log
	$(CMD) --models $(MODELS) --templates $(ERLANG) --out generated/erlang --clean
	cd generated/erlang && rebar3 fmt      && \
	               rebar3 clean    && \
	               rebar3 compile  && \
	               rebar3 dialyzer && \
	               rebar3 escriptize

erlang-debug: erlang
	# cd generated/erlang && \
	# erl -noshell -run \
	#               main uhppoted --debug \
	#                             --bind 192.168.1.100:0 \
	#                             --broadcast 192.168.1.255:60000 \
	#                             --listen 0.0.0.0:60001 \
	#                             get-controller yadda yadda2 yadda3\
	#                             -s init stop
	# 
	# cd generated/erlang && erl -noshell -run main uhppoted get-controller      -s init stop
	$(ERLBIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 set-door-passcodes

erlang-usage: erlang
	$(ERLBIN) 

erlang-all: erlang
	$(ERLBIN) all

erlang-listen: erlang
	$(ERLBIN) listen

lua: build regen
	$(CMD) --models $(MODELS) --templates $(LUA) --out generated/lua --clean
	# cd generated/lua && stylua ./**/*.lua

lua-help: build regen
	$(LUABIN) -h
	$(LUABIN) set-time -h 

lua-debug: lua
	# $(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 listen
	# $(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 get-controller --controller 405419896
	# $(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 put-card --controller 405419896 --card 10058400 --start-date 2023-01-02 --end-date 2023-12-30 --doors 1,2,4 --PIN 6310
	# $(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 set-time-profile --controller 405419896 --profile 37 --start-date 2023-01-01 --end-date 2023-12-31 --weekdays Mon,Tue,Sun --segments "8:30-11:45, 13:15-17:30" --linked 33
	# $(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 add-task --controller 405419896 --door 3 --task "door-normally-closed" --at "08:30" --start-date 2023-01-01 --end-date 2023-12-31 --weekdays Mon,Tue,Sun --more-cards 5
	$(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 clear-tasklist --controller 405419896

lua-all: lua
	$(LUABIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 --listen 0.0.0.0:60001 all
	
http: build
	$(CMD) --models $(MODELS) --templates $(HTTP) --out generated/http --clean
	npx eslint --fix generated/http/*.js 
