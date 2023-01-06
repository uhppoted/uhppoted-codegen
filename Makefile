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

GOBIN   = ./generated/go/bin/uhppoted
RUSTBIN = ./generated/rust/uhppoted/target/debug/uhppoted
PYBIN   = python3 ./generated/python/main.py
ZIGBIN  = ./generated/zig/zig-out/bin/uhppoted

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

vet: test
	go vet ./...

lint: vet
	golint ./...

benchmark: build
	go test -count 5 -bench=.  ./system/events

coverage: build
	go test -cover ./...

regen:
	$(CMD) export --models bindings/.models/models.json --tests bindings/.models/test-data.json

build-all: vet go rust python http zig
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

release: update-release build-all regen
	tar --directory=dist      --exclude=".DS_Store" -cvzf dist/$(DIST).tar.gz $(DIST)
	tar --directory=.         --exclude=".DS_Store" -cvzf dist/$(DIST)-bindings.tar.gz ./bindings
	tar --directory=generated --exclude=".DS_Store" --exclude="go/bin"               -cvzf dist/$(DIST)-go.tar.gz       go
	tar --directory=generated --exclude=".DS_Store" --exclude="rust/uhppoted/target" -cvzf dist/$(DIST)-rust.tar.gz     rust
	tar --directory=generated --exclude=".DS_Store" --exclude="python/__pycache__"   -cvzf dist/$(DIST)-python.tar.gz   python

publish: release
	echo "Releasing version $(VERSION)"
	rm -f dist/development-bindings.tar.gz
	rm -f dist/development-go.tar.gz
	rm -f dist/development-python.tar.gz
	rm -f dist/development-rust.tar.gz
	gh release create "$(VERSION)" ./dist/*.tar.gz --draft --prerelease --title "$(VERSION)-beta" --notes-file release-notes.md

# debug: go rust python http
# 	$(GOBIN)   --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)
# 	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)
# 	$(PYBIN)   --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)

debug: rust
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 get-controller
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 set-ip
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 get-time

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
	cd generated/go && go test ./uhppote

go-debug: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 $(COMMAND)

go-usage: regen build
	$(GOBIN)

go-all: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

go-listen: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 listen

rust: build regen 
	$(CMD) --models $(MODELS) --templates $(RUST) --out generated/rust
	cd generated/rust/uhppoted && cargo fmt && cargo build

rust-debug: rust
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 $(COMMAND)"

rust-usage: rust
	$(RUSTBIN)

rust-all: rust
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

rust-listen: rust
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 listen"

python: build regen 
	$(CMD) --models $(MODELS) --templates $(PYTHON) --out generated/python
	cd generated/python && yapf -ri .
	chmod +x generated/python/main.py

python-debug: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 $(COMMAND)

python-usage: python
	$(PYBIN)

python-all: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255 --listen 192.168.1.100:60001 all

python-listen: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 listen

http: build
	$(CMD) --models $(MODELS) --templates $(HTTP) --out generated/http --clean
	npx eslint --fix generated/http/*.js

zig: build regen
	$(CMD) --models $(MODELS) --templates $(ZIG) --out generated/zig --clean
	cd generated/zig && zig fmt src/* && zig build

zig-debug: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 clear-tasklist

zig-usage: zig
	$(ZIGBIN) 

zig-run: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

zig-listen: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 listen
