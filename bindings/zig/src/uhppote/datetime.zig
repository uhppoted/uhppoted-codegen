const std = @import("std");

const c = @cImport({
   @cDefine("_NO_CRT_STDIO_INLINE", "1");
   @cInclude("time.h");
});

pub const Date = struct {
    year: u16,
    month: u8,
    day: u8,
};

pub const Time = struct {
    hour: u8,
    minute: u8,
    second: u8,
};

pub const DateTime = struct {
    year: u16,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
    second: u8,
};

pub fn now() DateTime {
    const t = c.time(null);
    const tm = c.localtime(&t);
    const year: u16 = @intCast(tm.*.tm_year);
    const month: u8 = @intCast(tm.*.tm_mon);
    const day = tm.*.tm_mday;
    const hour = tm.*.tm_hour;
    const minute = tm.*.tm_min;
    const second = tm.*.tm_sec;

    return DateTime{
        .year = 1900 + year,
        .month = 1 + month,
        .day = @intCast(day),
        .hour = @intCast(hour),
        .minute = @intCast(minute),
        .second = @intCast(second),
    };
}
