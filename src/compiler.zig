const std = @import("std");
const scanner = @import("scanner.zig");

pub fn compile(source: []const u8, allocator: std.mem.Allocator) !void {
    const s = try scanner.Scanner.init(source, allocator);
    _ = s;
}
