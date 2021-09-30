const std = @import("std");

const Traps = enum(u16) {
    GETC = 0x20,   // 0: Read a single character
    OUT = 0x21,    // Write character to console
    PUTS = 0x22,   // Write string to console
    IN = 0x23,     // Print prompt and read
    PUTSP = 0x24,  //
    HALT = 0x25,   // Halt execution and print message to console

    pub fn val(self: Traps) u16 {
        return @enumToInt(self);
    }

    fn getC() void {
        std.debug.print("GETC trap called\n", .{});
    }

    fn out() void {
        std.debug.print("OUT trap called\n", .{});
    }

    fn puts() void {
        std.debug.print("PUTS trap called\n", .{});
    }

    fn in() void {
        std.debug.print("IN trap called\n", .{});
    }

    fn putsp() void {
        std.debug.print("PUTSP trap called\n", .{});
    }

    fn halt() void {
        std.debug.print("HALT trap called\n", .{});
    }
};

pub fn run(vector: u16) void {
    switch (vector) {
        Traps.GETC.val() => Traps.getC(),
        Traps.OUT.val() => Traps.out(),
        Traps.PUTS.val() => Traps.puts(),
        Traps.IN.val() => Traps.in(),
        Traps.PUTSP.val() => Traps.putsp(),
        Traps.HALT.val() => Traps.halt(),
        else => {
            std.debug.print("Did not understand trap vector: {b}\n", .{ vector });
        }
    }
}

