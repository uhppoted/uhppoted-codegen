DIST   ?= development
DEBUG  ?= --debug
CMD     = ./bin/uhppoted-codegen

MODELS     = languages/.models
GO         = languages/go
RUST       = languages/rust
JAVASCRIPT = languages/javascript

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
	go mod tidy

update-release:
	go get -u github.com/uhppoted/uhppote-core
	go mod tidy

format: 
	go fmt ./cmd/...
	go fmt ./codegen/...
	go fmt ./commands/...

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

build-all: vet
	mkdir -p dist/$(DIST)/windows
	mkdir -p dist/$(DIST)/darwin
	mkdir -p dist/$(DIST)/linux
	mkdir -p dist/$(DIST)/arm7
	env GOOS=linux   GOARCH=amd64       GOWORK=off go build -trimpath -o dist/$(DIST)/linux   ./...
	env GOOS=linux   GOARCH=arm GOARM=7 GOWORK=off go build -trimpath -o dist/$(DIST)/arm7    ./...
	env GOOS=darwin  GOARCH=amd64       GOWORK=off go build -trimpath -o dist/$(DIST)/darwin  ./...
	env GOOS=windows GOARCH=amd64       GOWORK=off go build -trimpath -o dist/$(DIST)/windows ./...

release: update-release build-all
	find . -name ".DS_Store" -delete
	tar --directory=dist --exclude=".DS_Store" -cvzf dist/$(DIST).tar.gz $(DIST)
	cd dist;  zip --recurse-paths $(DIST).zip $(DIST)

debug: build

delve: build
#	dlv exec ./bin/uhppoted-codegen
	dlv test github.com/uhppoted/uhppoted-codegen -- run Test*

version: build
	$(CMD) version

help: build
	$(CMD) help
	$(CMD) help commands
	$(CMD) help version
	$(CMD) help help

go: build
	$(CMD) --models $(MODELS) --templates $(GO) --out generated/go --clean
	cd generated/go && go fmt ./... && go mod tidy && go build main.go

go-debug: go
	./generated/go/main --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 get-time

go-all: go
	./generated/go/main --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 all

go-usage: build
	./generated/go/main

rust: build
	$(CMD) --models $(MODELS) --templates $(RUST) --out generated/rust
	cd generated/rust/uhppoted && cargo fmt && cargo build

rust-debug: rust
	./generated/rust/uhppoted/target/debug/uhppoted --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 get-time

rust-all: rust
	./generated/rust/uhppoted/target/debug/uhppoted --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 all

rust-usage: rust
	./generated/rust/uhppoted/target/debug/uhppoted

javascript: build
	$(CMD) --models $(MODELS) --templates $(JAVASCRIPT) --out generated/javascript --clean

