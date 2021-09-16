const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) void {
    const exe = b.addExecutable("lc3vm", "src/main.zig");
    exe.install();
}