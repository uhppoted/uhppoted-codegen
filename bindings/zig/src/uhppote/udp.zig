const std = @import("std");
const network = @import("zig-network");

const READ_TIMEOUT = 5000 * std.time.us_per_ms;
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

fn read_all(socket: *network.Socket, allocator: std.mem.Allocator) ![][64]u8 {
    const start = std.time.milliTimestamp();

    var replies = std.ArrayList([64]u8).init(allocator);
    defer replies.deinit();

    if (socket.setReadTimeout(100 * std.time.us_per_ms)) {
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
                    const dt = std.time.milliTimestamp() - start;

                    if (dt < 2500) {
                        // std.time.sleep(100 * std.time.ns_per_ms);
                        //                      waitForTime(100);
                    } else {
                        break;
                    }
                },
                else => {
                    std.debug.print("{any}\n", .{err});
                    break;
                },
            }
        }
    } else |err| {
        std.debug.print("{any}\n", .{err});
    }

    return replies.toOwnedSlice();
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
