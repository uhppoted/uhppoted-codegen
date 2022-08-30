DIST    ?= development
DEBUG   ?= --debug
CMD      = ./bin/uhppoted-codegen
COMMAND ?= listen

MODELS = languages/.models
GO     = languages/go
RUST   = languages/rust
PYTHON = languages/python
HTTP   = languages/http

GOBIN   = ./generated/go/bin/uhppoted
RUSTBIN = ./generated/rust/uhppoted/target/debug/uhppoted
PYBIN   = python3 ./generated/python/main.py

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
	go fmt ./commands/...
	go fmt ./codegen/...
	go fmt ./model/...

build: format
	go build -trimpath -o bin/ ./...

debug: go rust python http
	$(GOBIN)   --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)
	$(RUSTBIN) --debug --bind 192.168.1.100:0 --broadcast 192.168.1.255:60000 $(COMMAND)
	$(PYBIN)   --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)

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
	$(CMD) export --models languages/.models/models.json --tests languages/.models/test-data.json

build-all: vet
	mkdir -p dist/$(DIST)/windows
	mkdir -p dist/$(DIST)/darwin
	mkdir -p dist/$(DIST)/linux
	mkdir -p dist/$(DIST)/arm7
	env GOOS=linux   GOARCH=amd64       GOWORK=off go build -trimpath -o dist/$(DIST)/linux   ./...
	env GOOS=linux   GOARCH=arm GOARM=7 GOWORK=off go build -trimpath -o dist/$(DIST)/arm7    ./...
	env GOOS=darwin  GOARCH=amd64       GOWORK=off go build -trimpath -o dist/$(DIST)/darwin  ./...
	env GOOS=windows GOARCH=amd64       GOWORK=off go build -trimpath -o dist/$(DIST)/windows ./...

release: update-release build-all regen
	find . -name ".DS_Store" -delete
	tar --directory=dist --exclude=".DS_Store" -cvzf dist/$(DIST).tar.gz $(DIST)
	cd dist;  zip --recurse-paths $(DIST).zip $(DIST)

version: build
	$(CMD) version

help: build
	$(CMD) help
	$(CMD) help commands
	$(CMD) help version
	$(CMD) help help

export: build
	$(CMD) export --models runtime/models.json --tests runtime/test-data.json

go: build regen
	$(CMD) --models $(MODELS) --templates $(GO) --out generated/go --clean
	cd generated/go && go fmt ./... && go mod tidy && go build -o ./bin/ ./...

go-debug: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)

go-all: go
	$(GOBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 all

go-usage: regen build
	$(GOBIN)

rust: build regen 
	$(CMD) --models $(MODELS) --templates $(RUST) --out generated/rust
	cd generated/rust/uhppoted && cargo fmt && cargo build

rust-debug: rust
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)

rust-all: rust
	$(RUSTBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 all

rust-usage: rust
	$(RUSTBIN)

python: build regen 
	$(CMD) --models $(MODELS) --templates $(PYTHON) --out generated/python
	cd generated/python && yapf -ri .
	chmod +x generated/python/main.py

python-debug: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255:60000 $(COMMAND)

python-all: python
	$(PYBIN) --debug --bind 192.168.1.100 --broadcast 192.168.1.255 all

python-usage: python
	$(PYBIN)

http: build
	$(CMD) --models $(MODELS) --templates $(HTTP) --out generated/http --clean
	npx eslint --fix generated/http/*.js

