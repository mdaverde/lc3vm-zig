const std = @import("std");
const mem = @import("./mem.zig");

pub fn main() void {
    // const PC_START = 0x3000;

    // var running: bool = true;
    // const rl = reg.len;
    mem.memory[1] = 12;
    mem.reg[1] = 15;
    std.debug.print("{d} {d}\n", .{ mem.memory[1], mem.reg[1] });
}
