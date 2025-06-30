const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const value_mod = @import("value.zig");
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;
const Value = value_mod.Value;
const build_mode = @import("builtin").mode;

const STACK_MAX_SIZE = 256;

pub const InterpreterResult = enum {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
};

pub const BinOp = enum {
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: ArrayList(Value),
    alloc: Allocator,

    pub fn init(allocator: Allocator) VM {
        var vm = VM{ .chunk = undefined, .ip = undefined, .stack = ArrayList(Value).init(allocator), .alloc = allocator };
        vm.resetStack();
        return vm;
    }
    pub fn deinit(self: *VM) void {
        self.stack.deinit();
    }
    fn resetStack(self: *VM) void {
        self.stack.clearRetainingCapacity();
    }
    fn push(self: *VM, val: Value) !void {
        try self.stack.append(val);
    }
    fn pop(self: *VM) Value {
        const val = self.stack.pop();
        if (val == null) {
            @panic("Poped from empty stack");
        }
        return val.?;
    }

    pub fn interpret(vm: *VM, source: []const u8) InterpreterResult {
        var chunk = Chunk.init(vm.alloc);
        compiler.compile(source, &chunk, vm.alloc) catch return InterpreterResult.INTERPRET_COMPILE_ERROR;
        vm.chunk = &chunk;
        vm.ip = vm.chunk.code.items.ptr;
        return vm.run() catch InterpreterResult.INTERPRET_RUNTIME_ERROR;
    }

    fn run(vm: *VM) !InterpreterResult {
        while (true) {
            if (build_mode == .Debug) {
                std.debug.print("        ", .{});
                for (vm.stack.items) |slot| {
                    std.debug.print("[ ", .{});
                    debug.printValue(slot);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(vm.chunk.*, vm.ip - vm.chunk.code.items.ptr);
            }

            const instruction = readByte(vm);
            switch (instruction) {
                @intFromEnum(OpCode.OP_RETURN) => {
                    debug.printValue(vm.pop());
                    std.debug.print("\n", .{});
                    return InterpreterResult.INTERPRET_OK;
                },
                @intFromEnum(OpCode.OP_CONSTANT) => {
                    const constant = readConstant(vm);
                    try vm.push(constant);
                    // debug.printValue(value);
                    // std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_NEGATE) => try vm.push(-vm.pop()),
                @intFromEnum(OpCode.OP_ADD) => try vm.binOp(BinOp.PLUS),
                @intFromEnum(OpCode.OP_SUBSTRUCT) => try vm.binOp(BinOp.MINUS),
                @intFromEnum(OpCode.OP_MULTIPLY) => try vm.binOp(BinOp.MULTIPLY),
                @intFromEnum(OpCode.OP_DIVIDE) => try vm.binOp(BinOp.DIVIDE),
                else => {
                    std.debug.print("Unkown instruction {d}\n", .{instruction});
                    @panic("Error");
                },
            }
        }
    }

    inline fn readByte(vm: *VM) u8 {
        const ret = vm.ip[0];
        vm.ip += 1;
        return ret;
    }

    inline fn readConstant(vm: *VM) Value {
        return vm.chunk.constants.values.items[readByte(vm)];
    }

    inline fn binOp(vm: *VM, comptime operator: BinOp) !void {
        const op_right = vm.pop();
        const op_left = vm.pop();
        switch (operator) {
            BinOp.PLUS => try vm.push(op_left + op_right),
            BinOp.MINUS => try vm.push(op_left - op_right),
            BinOp.MULTIPLY => try vm.push(op_left * op_right),
            BinOp.DIVIDE => try vm.push(op_left / op_right),
        }
    }
};
