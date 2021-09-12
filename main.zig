const std = @import("std");

var memory = [_]u16{0} ** 65536;
var reg = [_]u16{0} ** @enumToInt(Registers.R_COUNT);

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
    R_COUNT,

    fn val(self: Registers) u16 {
        return @enumToInt(self);
    }
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

    fn val(self: ConditionFlags) u16 {
        return @enumToInt(self);
    }
};

fn signExtend(x: u16, bit_count: u4) u16 {
    var a = x;
    if ((a >> (bit_count - 1)) & 1 == 1) {
        a |= (@intCast(u16, 0xFFFF) << bit_count);
    }
    return a;
}

test "signExtend" {
    const one: u16 = 0x0001;
    try std.testing.expectEqual(signExtend(one, 3), 1);
    try std.testing.expectEqual(signExtend(one, 1), 0xFFFF);

    const neg_eight: i16 = -8;
    const unsized_neg_eight = @bitCast(u16, neg_eight);
    try std.testing.expectEqual(signExtend(unsized_neg_eight, 2), 65528);
}

fn resetRegisters() void {
    var i: usize = 0;
    while (i < reg.len) {
        reg[i] = 0;
        i += 1;
    }
}

test "resetRegisters" {
    reg[0] = 10;
    const expected_empty = [_]u16{0} ** reg.len;
    const expected_with_change = [_]u16{10, 0, 0};
    try std.testing.expectEqualSlices(u16, expected_with_change[0..], reg[0..3]);
    resetRegisters();
    try std.testing.expectEqualSlices(u16, expected_empty[0..], reg[0..]);
}

fn updateFlags(result_register: u16) void {
    const r_cond_index = Registers.R_COND.val();
    if (reg[result_register] == 0) {
        reg[r_cond_index] = ConditionFlags.FL_ZRO.val();
    } else if (reg[result_register] >> 15 == 1) {
        reg[r_cond_index] = ConditionFlags.FL_NEG.val();
    } else {
        reg[r_cond_index] = ConditionFlags.FL_POS.val();
    }
}

test "updateFlags" {
    resetRegisters();
    const expected = [_]u16{0} ** 10;
    try std.testing.expectEqualSlices(u16, expected[0..], reg[0..]);
    reg[Registers.R_R0.val()] = signExtend(@bitCast(u16, @as(i16, -1)), 1);
    updateFlags(Registers.R_R0.val());
    try std.testing.expectEqual(reg[Registers.R_COND.val()], ConditionFlags.FL_NEG.val());
}

fn addOp(instruction: u16) void {
    const destination_register: u16 = (instruction >> 9) & 0b111;
    const first_operand_register: u16 = (instruction >> 6) & 0b111;
    const imm_mode: u16 = (instruction >> 5) & 1;
    if (imm_mode == 0) {
        const second_operand_register: u16 = instruction & 0b111;
        const sum: u16 = reg[first_operand_register] + reg[second_operand_register];
        reg[destination_register] = sum;
    } else {
        // Since in immediate mode, we directly get the value to commit
        // the add operation against but we only get 5 bits. We should
        // extend the sign (Two's complement) if we're going to
        // convert that value to 16 bits.
        const imm_operand: u16 = signExtend(instruction & 0b11111, 5);
        const sum: u16 = reg[first_operand_register] + imm_operand;
        reg[destination_register] = sum;
    }
    updateFlags(destination_register);
}

test "addOp" {
    resetRegisters();
    reg[1] = 8;
    reg[2] = 5;
    // Add these 2 registers and save to register 0
    const instruction_one: u16 = 0b0001000001000010;
    addOp(instruction_one);
    const expected_one = [_]u16{13, 8, 5};
    try std.testing.expectEqualSlices(u16, expected_one[0..], reg[0..3]);
    try std.testing.expectEqual(reg[Registers.R_COND.val()], ConditionFlags.FL_POS.val());

    resetRegisters();
    reg[4] = 50;
    reg[6] = 32;
    // Save the sum to register 7
    const instruction_two: u16 = 0b0001111100000110;
    addOp(instruction_two);
    const expected_two = [_]u16{50, 0, 32, 82};
    try std.testing.expectEqualSlices(u16, expected_two[0..], reg[4..8]);
    try std.testing.expectEqual(reg[Registers.R_COND.val()], ConditionFlags.FL_POS.val());

    resetRegisters();
    reg[5] = 41;
    // Adds -5 in immediate mode
    const instruction_three: u16 = 0b0001000011110101;
    addOp(instruction_three);
    const expected_three = [_]u16{65525, 0, 0, 0, 0, 41};
    try std.testing.expectEqualSlices(u16, expected_three[0..], reg[0..6]);
    try std.testing.expectEqual(reg[Registers.R_COND.val()], ConditionFlags.FL_NEG.val());
}

pub fn main() void {
    // const PC_START = 0x3000;

    // var running: bool = true;

    std.debug.print("{e}\n", .{ ConditionFlags.FL_ZRO });
    // const rl = reg.len;
    memory[1] = 12;
    reg[1] = 15;
    std.debug.print("{d} {d}\n", .{ memory[1], reg[1] });
}
