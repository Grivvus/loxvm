const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const value_mod = @import("value.zig");
const Value = value_mod.Value;
const ValueArray = value_mod.ValueArray;

pub const OpCode = enum(u8) {
    OP_RETURN,
    OP_CONSTANT,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    constants: ValueArray,

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{ .code = ArrayList(u8).init(allocator), .constants = ValueArray.init(allocator) };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn write(self: *Chunk, opcode: u8) !void {
        try self.code.append(opcode);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.constants.write(value);
        return self.constants.values.items.len - 1;
    }
};
