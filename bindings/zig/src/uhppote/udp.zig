const std = @import("std");
const network = @import("zig-network");
const errors = @import("errors.zig");

const READ_TIMEOUT = 2500 * std.time.us_per_ms;
const WRITE_TIMEOUT = 1000 * std.time.us_per_ms;
const BUFFER_SIZE = 1024;

var debug: bool = false;

pub fn set_debug(v: bool) void {
    debug = v;
}

pub fn broadcast(packet: [64]u8, allocator: std.mem.Allocator) ![][64]u8 {
    // let bind = BIND_ADDR.read()?;
    // let broadcast = BROADCAST_ADDR.read()?;

    var socket = try network.Socket.create(.ipv4, .udp);
    defer socket.close();

    try socket.setBroadcast(true);
    try socket.setWriteTimeout(WRITE_TIMEOUT);

    const bindAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.any },
        .port = 3000,
    };

    const destAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.broadcast },
        .port = 60000,
    };

    try socket.bind(bindAddr);

    const N = try socket.sendTo(destAddr, &packet);

    if (debug) {
        std.debug.print("   ... sent {any} bytes\n", .{N});
    }

    dump(packet);

    return try read_all(&socket, allocator);
}

pub fn send(packet: [64]u8, allocator: std.mem.Allocator) ![64]u8 {
    // let bind = BIND_ADDR.read()?;
    // let broadcast = BROADCAST_ADDR.read()?;

    var socket = try network.Socket.create(.ipv4, .udp);
    defer socket.close();

    try socket.setBroadcast(true);
    try socket.setWriteTimeout(WRITE_TIMEOUT);

    const bindAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.any },
        .port = 3000,
    };

    const destAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.broadcast },
        .port = 60000,
    };

    try socket.bind(bindAddr);

    const N = try socket.sendTo(destAddr, &packet);

    if (debug) {
        std.debug.print("   ... sent {any} bytes\n", .{N});
    }

    dump(packet);

    return try read(&socket, allocator);
}

pub fn listen(_: std.mem.Allocator) !void {
    // let addr = LISTEN_ADDR.read()?;

    var socket = try network.Socket.create(.ipv4, .udp);
    defer socket.close();

    const listenAddr = network.EndPoint{
        .address = network.Address{ .ipv4 = network.Address.IPv4.any },
        .port = 60001,
    };

    try socket.bind(listenAddr);

    while (true) {
        var msg: [BUFFER_SIZE]u8 = undefined;
        if (socket.receiveFrom(&msg)) |reply| {
            if (debug) {
                std.debug.print("   ... received {any} bytes\n", .{reply.numberOfBytes});
            }

            if (reply.numberOfBytes == 64) {
                dump(msg[0..64].*);
            }
        } else |err| {
            std.debug.print("{any}\n", .{err});
            break;
        }
    }
}

fn read_all(socket: *network.Socket, allocator: std.mem.Allocator) ![][64]u8 {
    try socket.setReadTimeout(100 * std.time.us_per_ms);

    const start = std.time.microTimestamp();
    var replies = std.ArrayList([64]u8).init(allocator);
    defer replies.deinit();

    while (true) {
        var msg: [BUFFER_SIZE]u8 = undefined;
        if (socket.receiveFrom(&msg)) |reply| {
            if (debug) {
                std.debug.print("   ... received {any} bytes\n", .{reply.numberOfBytes});
            }

            if (reply.numberOfBytes == 64) {
                const slice: [64]u8 = msg[0..64].*;
                if (replies.append(slice)) {
                    dump(msg[0..64].*);
                } else |err| {
                    std.debug.print("{any}\n", .{err});
                }
            }
        } else |err| switch (err) {
            error.WouldBlock => {
                const dt = std.time.microTimestamp() - start;

                if (dt >= READ_TIMEOUT) {
                    break;
                }
            },
            else => {
                std.debug.print("{any}\n", .{err});
                break;
            },
        }
    }

    return replies.toOwnedSlice();
}

fn read(socket: *network.Socket, _: std.mem.Allocator) ![64]u8 {
    try socket.setReadTimeout(READ_TIMEOUT);

    while (true) {
        var msg: [BUFFER_SIZE]u8 = undefined;

        if (socket.receiveFrom(&msg)) |reply| {
            if (debug) {
                std.debug.print("   ... received {any} bytes\n", .{reply.numberOfBytes});
            }

            if (reply.numberOfBytes == 64) {
                dump(msg[0..64].*);

                return msg[0..64].*;
            }
        } else |err| switch (err) {
            error.WouldBlock => {
                return errors.UhppoteError.NoReply;
            },
            else => {
                return err;
            },
        }
    }
}

fn dump(packet: [64]u8) void {
    if (debug) {
        const offsets = [_]usize{ 0, 16, 32, 48 };
        for (offsets) |ix| {
            const f = "   {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2}";
            const u = packet[ix .. ix + 8];
            const v = packet[ix + 8 .. ix + 16];
            std.debug.print(f, .{ u[0], u[1], u[2], u[3], u[4], u[5], u[6], u[7] });
            std.debug.print("  ", .{});
            std.debug.print(f, .{ v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7] });
            std.debug.print("\n", .{});
        }

        std.debug.print("\n", .{});
    }
}
