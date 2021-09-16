const std = @import("std");
const mem = @import("./mem.zig");

const Registers = mem.Registers;
const ConditionFlags = mem.ConditionFlags;

pub const Opcodes = enum(u16) {
    BR,   // branch
    ADD,  // add
    LD,   // load
    ST,   // store
    JSR,  // jump register
    AND,  // bitwise and
    LDR,  // load register
    STR,  // store register
    RTI,  // unused
    NOT,  // bitwise not
    LDI,  // load indirect
    STI,  // store indirect
    JMP,  // jump
    RES,  // reserved (unused)
    LEA,  // load effective address
    TRAP, // execute trap

    pub fn val(self: Opcodes) u16 {
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

    const neg_eight: i5 = -8;
    const neg_eight_u16: u16 = @as(u16, @bitCast(u5, neg_eight));
    try std.testing.expectEqual(signExtend(neg_eight_u16, 4), 0xFFF8);
}

fn updateFlags(register: u16) void {
    if (mem.reg[register] == 0) {
        mem.reg[Registers.COND.val()] = ConditionFlags.ZRO.val();
    } else if  (mem.reg[register] >> 15 == 1) {
        mem.reg[Registers.COND.val()] = ConditionFlags.NEG.val();
    } else {
        mem.reg[Registers.COND.val()] = ConditionFlags.POS.val();
    }
}

test "updateFlags" {
    mem.clearMemory();
    const expected = [_]u16{0} ** 10;
    try std.testing.expectEqualSlices(u16, expected[0..], mem.reg[0..]);
    mem.reg[Registers.R0.val()] = signExtend(@bitCast(u16, @as(i16, -1)), 1);
    updateFlags(Registers.R0.val());
    try std.testing.expectEqual(mem.reg[Registers.COND.val()], ConditionFlags.NEG.val());
}

pub fn addOp(instr: u16) void {
    const destination_register: u16 = (instr >> 9) & 0b111;
    const first_operand_register: u16 = (instr >> 6) & 0b111;
    const imm_mode: u16 = (instr >> 5) & 1;
    if (imm_mode == 0) {
        const second_operand_register: u16 = instr & 0b111;
        const sum: u16 = mem.reg[first_operand_register] + mem.reg[second_operand_register];
        mem.reg[destination_register] = sum;
    } else {
        // Since in immediate mode, we directly get the value to commit
        // the add operation against but we only get 5 bits. We should
        // extend the sign (Two's complement) if we're going to
        // convert that value to 16 bits.
        const imm_operand: u16 = signExtend(instr & 0b11111, 5);
        const sum: u16 = mem.reg[first_operand_register] + imm_operand;
        mem.reg[destination_register] = sum;
    }
    updateFlags(destination_register);
}

test "addOp" {
    mem.clearMemory();
    mem.reg[1] = 8;
    mem.reg[2] = 5;
    // Add these 2 registers and save to register 0
    const instruction_one: u16 = 0b0001000001000010;
    addOp(instruction_one);
    const expected_one = [_]u16{13, 8, 5};
    try std.testing.expectEqualSlices(u16, expected_one[0..], mem.reg[0..3]);
    try std.testing.expectEqual(mem.reg[Registers.COND.val()], ConditionFlags.POS.val());

    mem.clearMemory();
    mem.reg[4] = 50;
    mem.reg[6] = 32;
    // Save the sum to register 7
    const instruction_two: u16 = 0b0001111100000110;
    addOp(instruction_two);
    const expected_two = [_]u16{50, 0, 32, 82};
    try std.testing.expectEqualSlices(u16, expected_two[0..], mem.reg[4..8]);
    try std.testing.expectEqual (mem.reg[Registers.COND.val()], ConditionFlags.POS.val());

    mem.clearMemory();
    mem.reg[5] = 41;
    // Adds -5 in immediate mode
    const instruction_three: u16 = 0b0001000011110101;
    addOp(instruction_three);
    const expected_three = [_]u16{65525, 0, 0, 0, 0, 41};
    try std.testing.expectEqualSlices(u16, expected_three[0..], mem.reg[0..6]);
    try std.testing.expectEqual (mem.reg[Registers.COND.val()], ConditionFlags.NEG.val());
}


pub fn ldiOp(instr: u16) void {
    const destination_register = instr >> 9 & 0b111;
    const pc_offset9: u16 = signExtend(instr & 0x1FF, 9);
    const pc: u16 = mem.reg[Registers.PC.val()];
    mem.reg[destination_register] = mem.fetch(mem.fetch(pc_offset9 + pc));
    updateFlags(destination_register);
}

test "ldiOp" {
    mem.clearMemory();

    // LDI op to register 3 from pc_offset9 010010010
    mem.memory[147] = 91;
    mem.memory[146] = 147;
    const instruction_one = 0b1010011010010010;
    ldiOp(instruction_one);
    try std.testing.expectEqual(@as(u16, 91), mem.reg[Registers.R3.val()]);
}

pub fn andOp(instr: u16) void {
    const destination_register = instr >> 9 & 0b111;
    const source_register_1 = instr >> 6 & 0b111;
    const imm_mode = instr >> 5 & 1;

    if (imm_mode == 0) {
        const source_register_2 = instr & 0b111;
        mem.reg[destination_register] = mem.reg[source_register_1] & mem.reg[source_register_2];
    } else {
        // imm_mode
        const imm_value = signExtend(instr & 0x1F, 5);
        mem.reg[destination_register] = mem.reg[source_register_1] & imm_value;
    }

    updateFlags(destination_register);
}

test "andOp" {
    mem.clearMemory();

    // mem.reg 3 = mem.reg 2 AND mem.reg 1
    const test_instruction1 = 0b0101011010000001;
    mem.reg[Registers.R2.val()] = 0b011;
    mem.reg[Registers.R1.val()] = 0b110;
    andOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 0b010), mem.reg[Registers.R3.val()]);

    mem.clearMemory();
    // mem.reg 0 = mem.reg 1 AND 10101;
    const test_instruction2 = 0b0101000001110101;
    mem.reg[Registers.R1.val()] = 60000;
    andOp(test_instruction2);
    try std.testing.expectEqual(@as(u16, 0b1110101001100000), mem.reg[Registers.R0.val()]);
}

pub fn brOp(instr: u16) void {
    const negative_conditional = instr >> 11 & 1;
    const zero_conditional = instr >> 10 & 1;
    const positive_conditional = instr >> 9 & 1;
    const last_instruction_flag = mem.reg[Registers.COND.val()];

    const is_negative = (negative_conditional == 1) and (last_instruction_flag == ConditionFlags.NEG.val());
    const is_zero = (zero_conditional == 1) and (last_instruction_flag == ConditionFlags.ZRO.val());
    const is_positive = (positive_conditional == 1) and (last_instruction_flag == ConditionFlags.POS.val());

    if (is_negative or is_zero or is_positive) {
        const pc_offset9 = instr & 0x1FF;
        mem.reg[Registers.PC.val()] = mem.reg[Registers.PC.val()] + signExtend(pc_offset9, 9);
    }
}

test "brOp" {
    mem.clearMemory();

    const test_instruction1 = 0b0000010000010000;
    mem.reg[Registers.COND.val()] =  ConditionFlags.ZRO.val();
    brOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 0b000010000), mem.reg[Registers.PC.val()]);
}

pub fn rtiOp(_: u16) void {
    std.os.abort();
}

pub fn resOp(_: u16) void {
    std.os.abort();
}

pub fn jmpOp(instr: u16) void {
    const base_register = instr >> 6 & 0b111;
    mem.reg[Registers.PC.val()] = mem.reg[base_register];
}

// special case of jmp
pub fn retOp() void {
    jmpOp(0b0000000111000000);
}

test "jmpOp" {
    mem.clearMemory();
    mem.reg[Registers.R3.val()] = 0b0000111100000000;
    const test_instruction1 = 0b1100000011000000;
    jmpOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 0b0000111100000000), mem.reg[Registers.PC.val()]);
}

pub fn jsrOp(instr: u16) void {
    const current_pc = mem.reg[Registers.PC.val()];
    mem.reg[Registers.R7.val()] = current_pc;
    const offset_mode = instr >> 11 & 1;
    if (offset_mode == 0) {
        const base_register = instr >> 6 & 0x7;
        mem.reg[Registers.PC.val()] = mem.reg[base_register];
    } else {
        const pc_offset11 = instr & 0x7FF;
        mem.reg[Registers.PC.val()] = current_pc + signExtend(pc_offset11, 11);
    }
}

test "jsrOp" {
    mem.clearMemory();

    mem.reg[Registers.R3.val()] = 22222;
    const test_instruction1 = 0b0100000011000000;
    jsrOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 22222), mem.reg[Registers.PC.val()]);

    mem.clearMemory();

    mem.reg[Registers.PC.val()] = 80;
    const test_instruction2 = 0b0100100000001111;
    jsrOp(test_instruction2);
    try std.testing.expectEqual(@as(u16, 80), mem.reg[Registers.R7.val()]);
    try std.testing.expectEqual(@as(u16, 95), mem.reg[Registers.PC.val()]);
}

pub fn ldOp(instr: u16) void {
    const destination_register = instr >> 9 & 0x7;
    const pc_offset9 = instr & 0xFF;
    const current_pc = mem.reg[Registers.PC.val()];
    mem.reg[destination_register] = mem.fetch(current_pc + signExtend(pc_offset9, 9));
    updateFlags(destination_register);
}

test "ldOp" {
    mem.clearMemory();

    mem.memory[15] = 151;
    const test_instruction1 = 0b0010100000001111;
    ldOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 151), mem.reg[Registers.R4.val()]);
}

pub fn ldrOp(instr: u16) void {
    const destination_register = instr >> 9 & 0x7;
    const base_register = instr >> 6 & 0x7;
    const offset6 = signExtend(instr & 0x3F, 6);
    mem.reg[destination_register] = mem.fetch(mem.reg[base_register] + offset6);
    updateFlags(destination_register);
}

test "ldrOp" {
    mem.clearMemory();

    mem.memory[32118] = 6;
    mem.reg[Registers.R3.val()] = 32100;
    const test_instruction1 = 0b0110101011010010;
    ldrOp(test_instruction1);
    try std.testing.expectEqual(@as(u16, 6), mem.reg[Registers.R5.val()]);
}

pub fn trapOp(instr: u16) void {
    mem.reg[Registers.R7.val()] = mem.reg[Registers.PC.val()];
    const trap_vector = instr & 0xFF;
    const start_address = mem.fetch(trap_vector);
    mem.reg[Registers.PC.val()] = start_address;
}

test "trapOp" {

}

pub fn stOp() void {

}

fn assert_instr_fn(comptime instr_fn: anytype) void {
    // try std.testing.expect(@typeInfo(instr_fn).Fn.args.len == 1);
    // try std.testing.expect(@typeInfo(instr_fn).Fn.args[0].arg_type.? == u16);
    try std.testing.expectEqual(@typeInfo(@TypeOf(instr_fn)), @typeInfo(@TypeOf(trapOp)));
}

comptime {
    assert_instr_fn(addOp);
    assert_instr_fn(brOp);
    assert_instr_fn(ldOp);
    assert_instr_fn(jsrOp);
    assert_instr_fn(andOp);
    assert_instr_fn(ldrOp);
    assert_instr_fn(rtiOp);
    assert_instr_fn(ldiOp);
    assert_instr_fn(jmpOp);
    assert_instr_fn(resOp);
    assert_instr_fn(trapOp);
}
