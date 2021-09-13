const std = @import("std");
const mem = @import("./mem.zig");

const Registers = mem.Registers;
const ConditionFlags = mem.ConditionFlags;

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

    const neg_eight: i5 = -8;
    const neg_eight_u16: u16 = @as(u16, @bitCast(u5, neg_eight));
    try std.testing.expectEqual(signExtend(neg_eight_u16, 4), 0xFFF8);
}

fn resetRegisters() void {
    var i: usize = 0;
    while (i < mem.reg.len) {
        mem.reg[i] = 0;
        i += 1;
    }
}

test "resetRegisters" {
    mem.reg[0] = 10;
    const expected_empty = [_]u16{0} ** mem.reg.len;
    const expected_with_change = [_]u16{10, 0, 0};
    try std.testing.expectEqualSlices(u16, expected_with_change[0..], mem.reg[0..3]);
    resetRegisters();
    try std.testing.expectEqualSlices(u16, expected_empty[0..], mem.reg[0..]);
}

fn updateFlags(result_register: u16) void {
    const r_cond_index = Registers.R_COND.val();
    if (mem.reg[result_register] == 0) {
        mem.reg[r_cond_index] = ConditionFlags.FL_ZRO.val();
    } else if  (mem.reg[result_register] >> 15 == 1) {
        mem.reg[r_cond_index] = ConditionFlags.FL_NEG.val();
    } else {
        mem.reg[r_cond_index] = ConditionFlags.FL_POS.val();
    }
}

test "updateFlags" {
    resetRegisters();
    const expected = [_]u16{0} ** 10;
    try std.testing.expectEqualSlices(u16, expected[0..], mem.reg[0..]);
    mem.reg[Registers.R_R0.val()] = signExtend(@bitCast(u16, @as(i16, -1)), 1);
    updateFlags(Registers.R_R0.val());
    try std.testing.expectEqual(mem.reg[Registers.R_COND.val()], ConditionFlags.FL_NEG.val());
}

fn addOp(instruction: u16) void {
    const destination_register: u16 = (instruction >> 9) & 0b111;
    const first_operand_register: u16 = (instruction >> 6) & 0b111;
    const imm_mode: u16 = (instruction >> 5) & 1;
    if (imm_mode == 0) {
        const second_operand_register: u16 = instruction & 0b111;
        const sum: u16 = mem.reg[first_operand_register] + mem.reg[second_operand_register];
        mem.reg[destination_register] = sum;
    } else {
        // Since in immediate mode, we directly get the value to commit
        // the add operation against but we only get 5 bits. We should
        // extend the sign (Two's complement) if we're going to
        // convert that value to 16 bits.
        const imm_operand: u16 = signExtend(instruction & 0b11111, 5);
        const sum: u16 = mem.reg[first_operand_register] + imm_operand;
        mem.reg[destination_register] = sum;
    }
    updateFlags(destination_register);
}

test "addOp" {
    resetRegisters();
    mem.reg[1] = 8;
    mem.reg[2] = 5;
    // Add these 2 registers and save to register 0
    const instruction_one: u16 = 0b0001000001000010;
    addOp(instruction_one);
    const expected_one = [_]u16{13, 8, 5};
    try std.testing.expectEqualSlices(u16, expected_one[0..], mem.reg[0..3]);
    try std.testing.expectEqual(mem.reg[Registers.R_COND.val()], ConditionFlags.FL_POS.val());

    resetRegisters();
    mem.reg[4] = 50;
    mem.reg[6] = 32;
    // Save the sum to register 7
    const instruction_two: u16 = 0b0001111100000110;
    addOp(instruction_two);
    const expected_two = [_]u16{50, 0, 32, 82};
    try std.testing.expectEqualSlices(u16, expected_two[0..], mem.reg[4..8]);
    try std.testing.expectEqual (mem.reg[Registers.R_COND.val()], ConditionFlags.FL_POS.val());

    resetRegisters();
    mem.reg[5] = 41;
    // Adds -5 in immediate mode
    const instruction_three: u16 = 0b0001000011110101;
    addOp(instruction_three);
    const expected_three = [_]u16{65525, 0, 0, 0, 0, 41};
    try std.testing.expectEqualSlices(u16, expected_three[0..], mem.reg[0..6]);
    try std.testing.expectEqual (mem.reg[Registers.R_COND.val()], ConditionFlags.FL_NEG.val());
}

// TBD
fn memRead(addr: u16) u16 {
    return addr;
}

fn ldiOp(instruction: u16) void {
    const destination_register = instruction >> 9 & 0b111;
    const pc_offset9: u16 = signExtend(instruction & 0x1FF, 9);
    const pc: u16 = mem.reg[Registers.R_PC.val()];
    mem.reg[destination_register] = memRead(memRead(pc_offset9 + pc));
    updateFlags(destination_register);
}

test "ldiOp" {
    resetRegisters();

    // LDI op to register 3 from pc_offset9 010010010
    const instruction_one = 0b1010011010010010;
    ldiOp(instruction_one);
    try std.testing.expectEqual(@as(u16, 0b010010010), mem.reg[Registers.R_R3.val()]);
}

fn andOp(instruction: u16) void {
    const destination_register = instruction >> 9 & 0b111;
    const source_register_1 = instruction >> 6 & 0b111;
    const imm_mode = instruction >> 5 & 1;

    if (imm_mode == 0) {
        const source_register_2 = instruction & 0b111;
        mem.reg[destination_register] = mem.reg[source_register_1] & mem.reg[source_register_2];
    } else {
        // imm_mode
        const imm_value = signExtend(instruction & 0x1F, 5);
        mem.reg[destination_register] = mem.reg[source_register_1] & imm_value;
    }

    updateFlags(destination_register);
}

test "andOp" {
    resetRegisters();

    // mem.reg 3 = mem.reg 2 AND mem.reg 1
    const test_instruction1 = 0b0101011010000001;
    mem.reg[Registers.R_R2.val()] = 0b011;
    mem.reg[Registers.R_R1.val()] = 0b110;
    andOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 0b010), mem.reg[Registers.R_R3.val()]);

    resetRegisters();
    // mem.reg 0 = mem.reg 1 AND 10101;
    const test_instruction2 = 0b0101000001110101;
    mem.reg[Registers.R_R1.val()] = 60000;
    andOp(test_instruction2);
    try std.testing.expectEqual(@as(u16, 0b1110101001100000), mem.reg[Registers.R_R0.val()]);
}

fn brOp(instr: u16) void {
    const negative_conditional = instr >> 11 & 1;
    const zero_conditional = instr >> 10 & 1;
    const positive_conditional = instr >> 9 & 1;
    const last_instruction_flag = mem.reg[Registers.R_COND.val()];

    const is_negative = (negative_conditional == 1) and (last_instruction_flag == ConditionFlags.FL_NEG.val());
    const is_zero = (zero_conditional == 1) and (last_instruction_flag == ConditionFlags.FL_ZRO.val());
    const is_positive = (positive_conditional == 1) and (last_instruction_flag == ConditionFlags.FL_POS.val());

    if (is_negative or is_zero or is_positive) {
        const pc_offset9 = instr & 0x1FF;
        mem.reg[Registers.R_PC.val()] = mem.reg[Registers.R_PC.val()] + signExtend(pc_offset9, 9);
    }
}

test "brOp" {
    resetRegisters();

    const test_instruction1 = 0b0000010000010000;
    mem.reg[Registers.R_COND.val()] =  ConditionFlags.FL_ZRO.val();
    brOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 0b000010000), mem.reg[Registers.R_PC.val()]);
}

fn rtiOp(_: u16) void {
    std.os.abort();
}

fn resOp(_: u16) void {
    std.os.abort();
}

fn jmpOp(instr: u16) void {
    const base_register = instr >> 6 & 0b111;
    mem.reg[Registers.R_PC.val()] = mem.reg[base_register];
}

// special case of jmp
fn retOp() void {
    jmpOp(0b0000000111000000);
}

test "jmpOp" {
    resetRegisters();
    mem.reg[Registers.R_R3.val()] = 0b0000111100000000;
    const test_instruction1 = 0b1100000011000000;
    jmpOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 0b0000111100000000), mem.reg[Registers.R_PC.val()]);
}

fn jsrOp() void {

}