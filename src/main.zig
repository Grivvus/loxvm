const std = @import("std");

const bytecode = @import("bytecode.zig");
const debug = @import("debug.zig");
const Chunk = bytecode.Chunk;
const OpCodes = bytecode.OpCode;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer {
        const check = gpa.deinit();
        if (check == std.heap.Check.leak) {
            std.debug.print("Warning, found memory leaks in allocator\n", .{});
            @panic("Memory leaks found");
        } else {
            std.debug.print("No memory leaks found\n", .{});
        }
    }

    const allocator = gpa.allocator();
    var ch = Chunk.init(allocator);
    defer ch.deinit();
    const index = try ch.addConstant(1.2);
    const index_u8: u8 = @intCast(index);
    try ch.write(@intFromEnum(OpCodes.OP_CONSTANT));
    try ch.write(index_u8);
    try ch.write(@intFromEnum(OpCodes.OP_RETURN));
    debug.disassembleChunk(ch, "test chunk");
}
