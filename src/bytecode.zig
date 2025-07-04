const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ValueArray = value_mod.ValueArray;

/// todo: implement OP_CONSTANT_LONG instruction
pub const OpCode = enum(u8) {
    OP_RETURN,
    OP_NEGATE,
    OP_ADD,
    OP_SUBSTRUCT,
    OP_DIVIDE,
    OP_MULTIPLY,
    OP_CONSTANT,
    OP_NIL,
    OP_FALSE,
    OP_TRUE,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(usize),
    constants: ValueArray,

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{ .code = ArrayList(u8).init(allocator), .lines = ArrayList(usize).init(allocator), .constants = ValueArray.init(allocator) };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, opcode: u8, line: usize) !void {
        try self.code.append(opcode);
        try self.lines.append(line);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.write(value);
        return self.constants.values.items.len - 1;
    }
};
