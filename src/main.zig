const std = @import("std");
const ops = @import("./ops.zig");
const mem = @import("./mem.zig");

const Registers = mem.Registers;
const OpCodes = ops.Opcodes;

const PC = Registers.PC.val();

pub fn main() !void {
    const args = std.process.args();
    var posix_args = args.inner;
    if (posix_args.count < 2) {
        std.debug.print("USAGE: lc3vm image-file\n", .{});
        std.os.exit(1);
    }
    _ = posix_args.next() orelse std.os.exit(1);
    // TODO: need to handle absolute
    const image_file_relative = posix_args.next() orelse std.os.exit(1);

    const current_dir = std.fs.cwd();
    const open_flags = std.fs.File.OpenFlags { .read = true };
    const image_file = try current_dir.openFile(image_file_relative, open_flags);
    defer image_file.close();

    const reader = std.fs.File.reader(image_file);
    const u16_max = std.math.maxInt(u16);
    const origin = try reader.readIntBig(u16);

    // Works for now
    var mem_instr_index = origin;
    reading: while (mem_instr_index < u16_max) {
        mem.memory[mem_instr_index] = reader.readIntBig(u16) catch |err| {
            switch (err) {
                error.EndOfStream => break :reading,
                else => std.os.abort(),
            }
        };
        mem_instr_index += 1;
    }


    // const origin = reader.readBytesNoEof(std.math.maxInt(u16));

    // Start of memory is reserved for trap routines
    const PC_START = 0x3000;
    mem.reg[PC] = PC_START;

    const running = false;

    while (running)  {
        const current_pc = mem.reg[PC] + 1;
        const instr = mem.read(current_pc);
        mem.reg[PC] = current_pc;

        const op: u16 = instr >> 12;
        switch (op) {
            OpCodes.ADD.val() => ops.addOp(instr),
            OpCodes.BR.val() => ops.brOp(instr),
            OpCodes.LD.val() => ops.ldOp(instr),
            OpCodes.ST.val() => ops.stOp(instr),
            OpCodes.JSR.val() => ops.jsrOp(instr),
            OpCodes.AND.val() => ops.andOp(instr),
            OpCodes.LDR.val() => ops.ldrOp(instr),
            OpCodes.STR.val() => ops.strOp(instr),
            OpCodes.RTI.val() => ops.rtiOp(instr),
            OpCodes.NOT.val() => ops.notOp(instr),
            OpCodes.LDI.val() => ops.ldiOp(instr),
            OpCodes.STI.val() => ops.stiOp(instr),
            OpCodes.JMP.val() => ops.jmpOp(instr),
            OpCodes.RES.val() => ops.resOp(instr),
            OpCodes.LEA.val() => ops.leaOp(instr),
            OpCodes.TRAP.val() => ops.trapOp(instr),
            else => std.os.abort(),
        }
    }
}
