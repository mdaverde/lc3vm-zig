const std = @import("std");
const ops = @import("./ops.zig");
const mem = @import("./mem.zig");

const Registers = mem.Registers;
const OpCodes = ops.Opcodes;

const PC = Registers.PC.val();

pub fn main() void {
    // Start of memory is reserved for trap routines
    const PC_START = 0x3000;
    mem.reg[PC] = PC_START;

    const running = true;

    while (running)  {
        const current_pc = mem.reg[PC] + 1;
        const instr = mem.fetch(current_pc);
        mem.reg[PC] = current_pc;

        const op: u16 = instr >> 12;
        switch (op) {
            OpCodes.ADD.val() => ops.addOp(instr),
            OpCodes.BR.val() => ops.brOp(instr),
            OpCodes.LD.val() => ops.ldOp(instr),
            // OpCodes.ST.val() => ops.stOp(instr),
            OpCodes.JSR.val() => ops.jsrOp(instr),
            OpCodes.AND.val() => ops.andOp(instr),
            OpCodes.LDR.val() => ops.ldrOp(instr),
            // OpCodes.STR.val() => ops.strOp(instr),
            OpCodes.RTI.val() => ops.rtiOp(instr),
            // OpCodes.NOT.val() => ops.notOp(instr),
            OpCodes.LDI.val() => ops.ldiOp(instr),
            // OpCodes.STI.val() => ops.stiOp(instr),
            OpCodes.JMP.val() => ops.jmpOp(instr),
            OpCodes.RES.val() => ops.resOp(instr),
            // OpCodes.LEA.val() => ops.leaOp(instr),
            OpCodes.TRAP.val() => ops.trapOp(instr),
            else => std.os.abort(),
        }
    }
}
