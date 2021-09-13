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
