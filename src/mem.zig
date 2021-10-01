const std = @import("std");
const c = @cImport({
    @cInclude("time.h");
    @cInclude("sys/select.h");
    @cInclude("poll.h");
});

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

const MemoryMappedRegisters = enum(u16) {
    KBSR = 0xFE00,
    KBDR = 0xFE02,

    pub fn val(self: MemoryMappedRegisters) u16 {
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

fn checkKeyboard() bool {
    var readfds: c.fd_set = undefined;
    // Looks like Zig's c-translate can't convert these macros?
    // c.FD_ZERO(&readfds);
    // c.FD_SET(std.os.linux.STDIN_FILENO, &readfds);

    var timeout: c.timeval = undefined;
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;
    return c.select(1, &readfds, 0, 0, &timeout) != 0;
}

fn checkKeyboardPoll() bool {
    var fds: c.pollfd = c.pollfd {
        .fd = std.os.linux.STDIN_FILENO,
        .events = 0,
        .revents = 0,
    };
    return c.poll(&fds, 1, -1) != 0;
}

pub fn read(addr: u16) u16 {
    if (addr == MemoryMappedRegisters.KBSR.val()) {
        if (checkKeyboardPoll()) {
            memory[MemoryMappedRegisters.KBSR.val()] = 1 << 15;
            memory[MemoryMappedRegisters.KBDR.val()] = @as(u16, std.io.getStdIn().reader().readByte() catch unreachable);
        } else {
            memory[MemoryMappedRegisters.KBSR.val()] = 0;
        }

    }
    return memory[addr];
}

pub fn write(addr: u16, val: u16) void {
    memory[addr] = val;
}
