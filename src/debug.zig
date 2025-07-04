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

pub fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        print("   | ", .{});
    } else {
        print("{d:0>4} ", .{chunk.lines.items[offset]});
    }
    const instruction = chunk.code.items[offset];
    return switch (instruction) {
        @intFromEnum(OpCode.OP_RETURN) => simpleInstruction("OP_RETURN", offset),
        @intFromEnum(OpCode.OP_CONSTANT) => constantInstruction("OP_CONSTANT", chunk, offset),
        @intFromEnum(OpCode.OP_FALSE) => simpleInstruction("OP_FALSE", offset),
        @intFromEnum(OpCode.OP_TRUE) => simpleInstruction("OP_TRUE", offset),
        @intFromEnum(OpCode.OP_NIL) => simpleInstruction("OP_NIL", offset),
        @intFromEnum(OpCode.OP_NEGATE) => simpleInstruction("OP_NEGATE", offset),
        @intFromEnum(OpCode.OP_ADD) => simpleInstruction("OP_ADD", offset),
        @intFromEnum(OpCode.OP_SUBSTRUCT) => simpleInstruction("OP_SUBSTRUCT", offset),
        @intFromEnum(OpCode.OP_MULTIPLY) => simpleInstruction("OP_MULTIPLY", offset),
        @intFromEnum(OpCode.OP_DIVIDE) => simpleInstruction("OP_DIVIDE", offset),
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
    return offset + 2;
}

pub fn printValue(val: Value) void {
    switch (val.vt) {
        .NIL => print("nil", .{}),
        .BOOL => {
            if (val.asBoolean()) {
                print("true", .{});
            } else {
                print("false", .{});
            }
        },
        .NUMBER => print("{any}", .{val.asNumber()}),
    }
}

pub fn printStack(stack: []Value) void {
    print("   stack   \n", .{});
    for (stack, 0..) |val, i| {
        printValue(val);
        print(" on index {d}", .{i});
        print("  ", .{});
    }
    print("\n", .{});
    print("    stack end    \n", .{});
}
