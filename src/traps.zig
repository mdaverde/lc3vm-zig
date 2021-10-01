const std = @import("std");
const mem = @import("./mem.zig");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

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
        const input_char = stdin.readByte() catch unreachable;
        mem.reg[mem.Registers.R0.val()] = @as(u16, input_char);
    }

    fn out() void {
        const output_char = @truncate(u8, mem.reg[mem.Registers.R0.val()]);
        stdout.print("{c}", .{ output_char }) catch unreachable;
    }

    fn puts() void {
        const init_str_address = mem.reg[mem.Registers.R0.val()];
        var str_len: u16 = 0;

        var str_val_long: u16 = mem.memory[init_str_address];
        while (str_val_long != 0) {
            const str_val_char: u8 = @truncate(u8, str_val_long);
            stdout.print("{c}", .{ str_val_char }) catch unreachable;
            str_len += 1;
            str_val_long = mem.memory[init_str_address + str_len];
        }
    }

    fn in() void {
        stdout.print("Enter a character: ", .{}) catch unreachable;
        const input_char = stdin.readByte() catch unreachable;
        mem.reg[mem.Registers.R0.val()] = @as(u16, input_char);
        stdout.print("{c}", .{ input_char }) catch unreachable;
    }

    fn putsp() void {
        std.debug.print("PUTSP trap called\n", .{});

        const str_addr = mem.reg[mem.Registers.R0.val()];
        var str_len: u16 = 0;
        var double_char = mem.memory[str_addr];
        while (double_char != 0) {
            const first_char_long = double_char & 0xFF;
            const first_char = @truncate(u8, first_char_long);
            stdout.print("{c}", .{ first_char }) catch unreachable;
            const second_char_long = double_char >> 8;
            if (second_char_long != 0) {
                const second_char = @truncate(u8, second_char_long);
                stdout.print("{c}", .{ second_char }) catch unreachable;
            }
            str_len += 1;
            double_char = mem.memory[str_addr + str_len];
        }
    }

    fn halt() void {
        stdout.print("HALT", .{}) catch unreachable;
        std.os.exit(0);
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

