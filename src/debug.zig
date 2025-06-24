const std = @import("std");
const print = std.debug.print;

const bytecode = @import("bytecode.zig");
const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;

pub fn disassembleChunk(chunk: Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = disassembleInstruction(chunk, offset);
    }
}

fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    const instruction = chunk.code.items[offset];
    return switch (instruction) {
        OpCode.OP_RETURN => simpleInstruction("OP_RETURN", offset),
    };
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}
