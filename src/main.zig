const std = @import("std");
const chunk = @import("chunk.zig");
const Chunk = chunk.Chunk;
const OpCodes = chunk.OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    const allocator = gpa.allocator();
    var ch = chunk.Chunk.init(allocator);
    try ch.write(OpCodes.OP_RETURN);
}
