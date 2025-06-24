const std = @import("std");
const print = std.debug.print;

const bytecode = @import("bytecode.zig");
const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;
const value_mod = @import("value.zig");
const Value = value_mod.Value;

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
        @intFromEnum(OpCode.OP_RETURN) => simpleInstruction("OP_RETURN", offset),
        @intFromEnum(OpCode.OP_CONSTANT) => constantInstruction("OP_CONSTANT", chunk, offset),
        else => {
            print("Unkown opcode {d}\n", .{instruction});
            return offset + 1;
        },
    };
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    const constant = chunk.code.items[offset + 1];
    print("{s:<16} {d:>4} '", .{ name, constant });
    printValue(chunk.constants.values.items[constant]);
    print("'\n", .{});
    return offset + 1;
}

fn printValue(val: Value) void {
    print("{any}", .{val});
}
