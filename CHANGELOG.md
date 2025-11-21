# CHANGELOG

## Unreleased

### Updated
1. Augmented models with test vectors.
2. Reworked models with defined types for:
   - `door-mode`
   - `event-type`
   - `task`
   - `interlock`
3. Updated to Go 1.25.


## [0.8.11](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.11) - 2025-07-01

### Added
1. `get/set-antipassback` API function to manage the anti-passback mode for a controller.

### Updated
1. Updated to Go 1.24.


## [0.8.10](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.10) - 2025-01-30

### Updated
1. Added auto-send interval to get/set-listener API.


## [0.8.9](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.9) - 2024-09-06

### Added
1. Added support for transport over UDP connected sockets and TCP.

### Updated
1. Updated to Go 1.23.


## [0.8.8](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.8) - 2024-03-27

### Added
1. `restore-default-parameters` function to reset a controller to the manufacturer default configuration.

### Updated
1. Updated Go version to 1.22


## [0.8.7](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.7) - 2023-12-01

### Added
1. `set-door-passcodes` function.
2. _.ignore_ file for files to be skipped for code-generation.
3. _Lua_ bindings.

### Updated
1. Updated Zig to v0.11.0
2. Added unit tests to all bindings for decoding `get-status` response without an event.
3. Generated PyUnit unit tests from models.
4. Generated Zig unit tests from models.


## [0.8.6](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.6) - 2023-08-30

### Added
1. `activate-keypads` function.
2. _erlang_ bindings.

### Updated
1. Replaced os.Rename with lib implementation for tmpfs support.


## [0.8.5](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.5) - 2023-06-13

### Added
1. PHP bindings.
2. `set-interlock` function.

### Updated
1. Reworked Go language bindings to use zero value rather than pointer for optional Date 
   and DateTime fields.
2. Added static-check linting to CI build.


## [0.8.4](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.4) - 2023-03-17

### Added
1. Added Zig to bindings.
2. Overview Go doc.
3. Added set-pc-control to models and bindings.
4. Added card PIN field to models and bindings.


## [0.8.3](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.3) - 2022-12-16

### Added
1. Added ARM64 to release build artifacts


## [0.8.2](https://github.com/uhppoted/uhppoted-codegen/releases/tag/v0.8.2) - 2022-10-14

1. Initial release