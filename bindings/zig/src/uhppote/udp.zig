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

// queue = std.PriorityQueue(Delay, void, cmp).init(std.heap.page_allocator, undefined);
// defer queue.deinit();
//
// _ = async read_all(&socket);
// _ = async wait(&socket);
//
// while (queue.removeOrNull()) |delay| {
//     const now = nanotime();
//     if (now < delay.expires) {
//         std.time.sleep(delay.expires - now);
//     }
//
//     resume delay.frame;
// }
//
// var timer: ?std.time.Timer = null;
// fn nanotime() u64 {
//     if (timer == null) {
//         timer = std.time.Timer.start() catch unreachable;
//     }
//     return timer.?.read();
// }
//
// fn wait(socket: *network.Socket) void {
//     var tasks = [_]@Frame(close){
//         async close(2500, socket),
//     };
//
//     for (tasks) |*t| await t;
// }
//
// fn close(
//     time: u64,
//     socket: *network.Socket,
// ) void {
//     waitForTime(time);
//     std.debug.print("CLOSING {any}\n", .{socket});
//     socket.close();
// }
//
// const Delay = struct {
//     frame: anyframe,
//     expires: u64,
// };
//
// var queue: std.PriorityQueue(Delay, void, cmp) = undefined;
// fn cmp(context: void, a: Delay, b: Delay) std.math.Order {
//     _ = context;
//     return std.math.order(a.expires, b.expires);
// }
//
// fn waitForTime(time_ms: u64) void {
//     suspend queue.add(Delay{
//         .frame = @frame(),
//         .expires = nanotime() + (time_ms * std.time.ns_per_ms),
//     }) catch unreachable;
// }
