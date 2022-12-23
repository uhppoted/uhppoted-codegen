const std = @import("std");
const network = @import("zig-network");

const READ_TIMEOUT = 5000;
const WRITE_TIMEOUT = 1000;
const BUFFER_SIZE = 1024;

var debug: bool = false;

pub fn set_debug(v: bool) void {
    debug = v;
}

pub fn broadcast(packet: [64]u8) !void {
    // let bind = BIND_ADDR.read()?;
    // let broadcast = BROADCAST_ADDR.read()?;

    var socket = try network.Socket.create(.ipv4, .udp);

    defer socket.close();

    try socket.setBroadcast(true);
    // try socket.setTimeouts(READ_TIMEOUT, WRITE_TIMEOUT);

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
        std.debug.print("  ... sent {any} bytes\n", .{N});
    }

    dump(packet);

    var msg: [BUFFER_SIZE]u8 = undefined;

    while (true) {
        const reply = try socket.receiveFrom(&msg);

        if (debug) {
            std.debug.print("... received {any} bytes\n", .{reply.numberOfBytes});
        }

        if (reply.numberOfBytes == 64) {
            dump(msg[0..64].*);
        }
    }

    //    //     return read_all(socket).await;
}

fn dump(packet: [64]u8) void {
    if (debug) {
        const offsets = [_]usize{ 0, 16, 32, 48 };
        for (offsets) |ix| {
            const f = "{x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2} {x:0<2}";
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
