const std = @import("std");
const network = @import("zig-network");
const errors = @import("errors.zig");

const READ_TIMEOUT = 2500 * std.time.us_per_ms;
const WRITE_TIMEOUT = 1000 * std.time.us_per_ms;
const BUFFER_SIZE = 1024;

var bindAddr = network.EndPoint{
    .address = network.Address{ .ipv4 = network.Address.IPv4.any },
    .port = 0,
};

var broadcastAddr = network.EndPoint{
    .address = network.Address{ .ipv4 = network.Address.IPv4.broadcast },
    .port = 60000,
};

var listenAddr = network.EndPoint{
    .address = network.Address{ .ipv4 = network.Address.IPv4.any },
    .port = 60001,
};

var debug: bool = false;

pub fn set_bind_address(addr: [:0]const u8) !void {
    var address: network.Address = network.Address{ .ipv4 = network.Address.IPv4.any };
    var port: u16 = 0;
    var it = std.mem.split(u8, addr, ":");

    if (it.next()) |v| {
        address = network.Address{ .ipv4 = try network.Address.IPv4.parse(v) };
    }

    if (it.next()) |v| {
        port = try std.fmt.parseUnsigned(u16, v, 10);
    }

    bindAddr = network.EndPoint{
        .address = address,
        .port = port,
    };
}

pub fn set_broadcast_address(addr: [:0]const u8) !void {
    var address: network.Address = network.Address{ .ipv4 = network.Address.IPv4.broadcast };
    var port: u16 = 60000;
    var it = std.mem.split(u8, addr, ":");

    if (it.next()) |v| {
        address = network.Address{ .ipv4 = try network.Address.IPv4.parse(v) };
    }

    if (it.next()) |v| {
        port = try std.fmt.parseUnsigned(u16, v, 10);
    }

    broadcastAddr = network.EndPoint{
        .address = address,
        .port = port,
    };
}

pub fn set_listen_address(addr: [:0]const u8) !void {
    var address: network.Address = network.Address{ .ipv4 = network.Address.IPv4.any };
    var port: u16 = 60001;
    var it = std.mem.split(u8, addr, ":");

    if (it.next()) |v| {
        address = network.Address{ .ipv4 = try network.Address.IPv4.parse(v) };
    }

    if (it.next()) |v| {
        port = try std.fmt.parseUnsigned(u16, v, 10);
    }

    listenAddr = network.EndPoint{
        .address = address,
        .port = port,
    };
}

pub fn set_debug(v: bool) !void {
    debug = v;
}

pub fn broadcast(packet: [64]u8, allocator: std.mem.Allocator) ![][64]u8 {
    var socket = try network.Socket.create(.ipv4, .udp);
    defer socket.close();

    try socket.setBroadcast(true);
    try socket.setWriteTimeout(WRITE_TIMEOUT);
    try socket.bind(bindAddr);

    const N = try socket.sendTo(broadcastAddr, &packet);
    if (debug) {
        std.debug.print("   ... sent {any} bytes\n", .{N});
    }

    dump(packet);

    return try read_all(&socket, allocator);
}

pub fn send(packet: [64]u8, allocator: std.mem.Allocator) ![64]u8 {
    var socket = try network.Socket.create(.ipv4, .udp);
    defer socket.close();

    try socket.setBroadcast(true);
    try socket.setWriteTimeout(WRITE_TIMEOUT);
    try socket.bind(bindAddr);

    const N = try socket.sendTo(broadcastAddr, &packet);
    if (debug) {
        std.debug.print("   ... sent {any} bytes\n", .{N});
    }

    dump(packet);

    // set-ip does not return a response
    if (packet[1] == 0x96) { 
        return [_]u8{0} ** 64;
    } 

    return try read(&socket, allocator);    
}

pub fn listen(queue: *std.atomic.Queue([64]u8), allocator: std.mem.Allocator) !void {
    var socket = try network.Socket.create(.ipv4, .udp);
    defer socket.close();

    try socket.bind(listenAddr);

    while (true) {
        var msg: [BUFFER_SIZE]u8 = undefined;
        if (socket.receiveFrom(&msg)) |reply| {
            if (debug) {
                std.debug.print("   ... received {any} bytes\n", .{reply.numberOfBytes});
            }

            if (reply.numberOfBytes == 64) {
                dump(msg[0..64].*);

                const node = allocator.create(std.atomic.Queue([64]u8).Node);

                if (node) |n| {
                    n.* = .{
                        .prev = undefined,
                        .next = undefined,
                        .data = msg[0..64].*,
                    };

                    queue.put(n);
                } else |err| {
                    std.debug.print("\n   *** WARN   {any}\n", .{err});
                }
            }
        } else |err| {
            std.debug.print("{any}\n", .{err});
            break;
        }
    }
}

fn read_all(socket: *network.Socket, allocator: std.mem.Allocator) ![][64]u8 {
    try socket.setReadTimeout(100 * std.time.us_per_ms);

    const start = std.time.milliTimestamp() * std.time.us_per_ms;
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
                const dt = std.time.milliTimestamp()*std.time.us_per_ms - start;

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
                return errors.UhppotedError.NoReply;
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
            const f = "   {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2} {x:0>2}";
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
