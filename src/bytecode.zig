const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

pub const OpCode = enum(u8) {
    OP_RETURN,
};

pub const Chunk = struct {
    code: ArrayList(OpCode),

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{ .code = ArrayList(OpCode).init(allocator) };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
    }

    pub fn write(self: *Chunk, opcode: OpCode) !void {
        try self.code.append(opcode);
    }
};
