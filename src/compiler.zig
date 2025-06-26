const std = @import("std");
const scanner = @import("scanner.zig");

pub fn compile(source: []const u8) !void {
    const s = scanner.Scanner.init(source);
    _ = s;
}
