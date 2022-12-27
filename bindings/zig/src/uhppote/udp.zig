const std = @import("std");
const network = @import("zig-network");

const READ_TIMEOUT = 5000 * std.time.us_per_ms;
const WRITE_TIMEOUT = 1000 * std.time.us_per_ms;
const BUFFER_SIZE = 1024;

var debug: bool = false;

var queue: std.PriorityQueue(Delay, void, cmp) = undefined;
fn cmp(context: void, a: Delay, b: Delay) std.math.Order {
    _ = context;
    return std.math.order(a.expires, b.expires);
}

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

    queue = std.PriorityQueue(Delay, void, cmp).init(std.heap.page_allocator, undefined);
    defer queue.deinit();

    _ = async read_all(&socket);
    _ = async wait(&socket);

    while (queue.removeOrNull()) |delay| {
        const now = nanotime();
        if (now < delay.expires) {
            std.time.sleep(delay.expires - now);
        }

        resume delay.frame;
    }
}

fn wait(socket: *network.Socket) void {
    var tasks = [_]@Frame(close){
        async close(2500, socket),
    };

    for (tasks) |*t| await t;
}

fn close(
    time: u64,
    socket: *network.Socket,
) void {
    waitForTime(time);
    std.debug.print("CLOSING {any}\n", .{socket});
    socket.close();
}

fn read_all(socket: *network.Socket) void {
    var msg: [BUFFER_SIZE]u8 = undefined;

    if (socket.setReadTimeout(10 * std.time.us_per_ms)) {
        while (true) {
            if (socket.receiveFrom(&msg)) |reply| {
                if (debug) {
                    std.debug.print("... received {any} bytes\n", .{reply.numberOfBytes});
                }

                if (reply.numberOfBytes == 64) {
                    dump(msg[0..64].*);
                }
            } else |err| switch (err) {
                error.WouldBlock => {
                    waitForTime(250);
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

var timer: ?std.time.Timer = null;
fn nanotime() u64 {
    if (timer == null) {
        timer = std.time.Timer.start() catch unreachable;
    }
    return timer.?.read();
}

const Delay = struct {
    frame: anyframe,
    expires: u64,
};

fn waitForTime(time_ms: u64) void {
    suspend queue.add(Delay{
        .frame = @frame(),
        .expires = nanotime() + (time_ms * std.time.ns_per_ms),
    }) catch unreachable;
}
