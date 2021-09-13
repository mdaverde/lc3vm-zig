const std = @import("std");

pub var memory = [_]u16{0} ** 65536;
pub var reg = [_]u16{0} ** @enumToInt(Registers.COUNT);

pub const Registers = enum(u16) {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC, // program counter
    COND,
    COUNT,

    pub fn val(self: Registers) u16 {
        return @enumToInt(self);
    }
};

pub const ConditionFlags = enum(u16) {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,

    pub fn val(self: ConditionFlags) u16 {
        return @enumToInt(self);
    }
};

pub fn clearMemory() void {
    // Reset registers
    var i: usize = 0;
    while (i < reg.len) {
        reg[i] = 0;
        i += 1;
    }

    // Clear RAM
    i = 0;
    while (i < memory.len) {
        memory[i] = 0;
        i += 1;
    }
}

test "clearMemory" {
    reg[0] = 10;
    const expected_empty = [_]u16{0} ** reg.len;
    const expected_with_change = [_]u16{10, 0, 0};
    try std.testing.expectEqualSlices(u16, expected_with_change[0..], reg[0..3]);
    clearMemory();
    try std.testing.expectEqualSlices(u16, expected_empty[0..], reg[0..]);
}

pub fn fetch(addr: u16) u16 {
    return memory[addr];
}
