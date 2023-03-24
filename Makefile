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

.DEFAULT_GOAL := test
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

build-all: test vet lint go rust python http zig
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

publish: release
	echo "Releasing version $(VERSION)"
	gh release create "$(VERSION)" \
	"./dist/uhppoted-codegen_$(VERSION).tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-bindings.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-go.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-python.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-rust.tar.gz" \
	"./dist/uhppoted-codegen_$(VERSION)-zig.tar.gz" \
	--draft --prerelease --title "$(VERSION)-beta" --notes-file release-notes.md

debug: rust
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 get-controller
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 set-ip
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 get-time

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
	cd generated/go && go test ./uhppote

go-debug: go
	# $(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 $(COMMAND)
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 put-card
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 get-card
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 get-card-by-index

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
	# bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 $(COMMAND)"
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 put-card"
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 get-card"
	bash -c "exec -a uhppoted $(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 get-card-by-index"

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
	# $(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 $(COMMAND)
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 put-card
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 get-card
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 get-card-by-index

python-usage: python
	$(PYBIN)

python-all: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255 --listen 192.168.1.100:60001 all

python-listen: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 --listen 192.168.1.100:60001 listen

zig: build regen
	$(CMD) --models $(MODELS) --templates $(ZIG) --out generated/zig --clean
	cd generated/zig && zig fmt src/* && zig build

zig-debug: zig
	# $(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 $(COMMAND)
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 put-card
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 get-card
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 get-card-by-index

zig-usage: zig
	$(ZIGBIN) 

zig-all: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 all

zig-listen: zig
	$(ZIGBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000  --listen 192.168.1.100:60001 listen

http: build
	$(CMD) --models $(MODELS) --templates $(HTTP) --out generated/http --clean
	npx eslint --fix generated/http/*.js
