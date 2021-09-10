const std = @import("std");

const memory = [65536:0]u16;

const Registers = enum(u16) {
    R_R0,
    R_R1,
    R_R2,
    R_R3,
    R_R4,
    R_R5,
    R_R6,
    R_R7,
    R_PC, // program counter
    R_COND,
    R_COUNT
};

const Opcodes = enum(u16) {
    OP_BR,   // branch
    OP_ADD,  // add
    OP_LD,   // load
    OP_ST,   // store
    OP_JSR,  // jump register
    OP_AND,  // bitwise and
    OP_LDR,  // load register
    OP_STR,  // store register
    OP_RTI,  // unused
    OP_NOT,  // bitwise not
    OP_LDR,  // load indirect
    OP_STI,  // store indirect
    OP_JMP,  // jump
    OP_RES,  // reserved (unused)
    OP_LEA,  // load effective address
    OP_TRAP, // execute trap
};


const ConditionFlags = enum(u16) {
    FL_POS = 1 << 0,
    FL_ZRO = 1 << 1,
    FL_NEG = 1 << 2,
};

fn signExtend(x: u16, bit_count: u4) u16 {
    var a = x;
    if ((a >> (bit_count - 1)) & 1 == 1) {
        a |= (@intCast(u16, 0xFFFF) << bit_count);
    }
    return a;
}

test "sign extend" {
    const one: u16 = 0x0001;
    try std.testing.expectEqual(signExtend(one, 3), 1);
    try std.testing.expectEqual(signExtend(one, 1), 0xFFFF);

    // How do you convert a signed integer to an unsigned integer? (not keeping its value)
    const neg_eight: i16 = -8;
    const unsized_neg_eight = @bitCast(u16, neg_eight);
    std.debug.print("{b} {b}\n", .{ neg_eight, unsized_neg_eight });
    try std.testing.expectEqual(signExtend(unsized_neg_eight, 2), 0xFFFF);
}

pub fn main() void {
    // const PC_START = 0x3000;

    // var running: bool = true;

    std.debug.print("{e}\n", .{ ConditionFlags.FL_ZRO });
    std.debug.print("{d}\n", .{ 0b1010 });
}
