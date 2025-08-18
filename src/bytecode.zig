const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const value_mod = @import("value.zig");
const vm_mod = @import("vm.zig");
const VM = vm_mod.VM;
const Value = value_mod.Value;
const ValueArray = value_mod.ValueArray;

/// todo: implement OP_CONSTANT_LONG instruction
pub const OpCode = enum(u8) {
    OP_POP,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
    OP_SET_UPVALUE,
    OP_GET_UPVALUE,
    OP_GET_LOCAL,
    OP_SET_LOCAL,
    OP_SET_PROPERTY,
    OP_GET_PROPERTY,
    OP_DEFINE_GLOBAL,
    OP_PRINT,
    OP_JUMP,
    OP_JUMP_IF_FALSE,
    OP_RETURN,
    OP_NEGATE,
    OP_NOT,
    OP_ADD,
    OP_SUBSTRUCT,
    OP_DIVIDE,
    OP_MULTIPLY,
    OP_CONSTANT,
    OP_NIL,
    OP_FALSE,
    OP_TRUE,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_LOOP,
    OP_CALL,
    OP_CLOSURE,
    OP_CLOSE_UPVALUE,
    OP_CLASS,
    OP_METHOD,
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

    pub fn addConstant(self: *Chunk, vm: *VM, value: Value) !usize {
        vm.push(value);
        try self.constants.write(value);
        _ = vm.pop();
        return self.constants.values.items.len - 1;
    }

    pub inline fn count(self: *Chunk) usize {
        return self.code.items.len;
    }
};
