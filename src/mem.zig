pub var memory = [_]u16{0} ** 65536;
pub var reg = [_]u16{0} ** @enumToInt(Registers.R_COUNT);

pub const Registers = enum(u16) {
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

    pub fn val(self: Registers) u16 {
        return @enumToInt(self);
    }
};

pub const ConditionFlags = enum(u16) {
    FL_POS = 1 << 0,
    FL_ZRO = 1 << 1,
    FL_NEG = 1 << 2,

    pub fn val(self: ConditionFlags) u16 {
        return @enumToInt(self);
    }
};
