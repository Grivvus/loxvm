const std = @import("std");
const ArrayList = std.ArrayList;
const bytecode = @import("bytecode.zig");
const value_mod = @import("value.zig");
const debug = @import("debug.zig");
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
    stack: [STACK_MAX_SIZE]Value,
    stack_top: [*]Value,

    pub fn init(chunk: *Chunk) VM {
        var vm = VM{ .chunk = chunk, .ip = chunk.code.items.ptr, .stack = undefined, .stack_top = undefined };
        vm.resetStack();
        return vm;
    }
    pub fn deinit(self: *VM) void {
        _ = self;
    }
    fn resetStack(self: *VM) void {
        self.stack_top = &self.stack;
    }
    fn push(self: *VM, val: Value) void {
        self.stack_top[0] = val;
        self.stack_top += 1;
    }
    fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    pub fn interpret(vm: *VM, chunk: *Chunk) InterpreterResult {
        vm.chunk = chunk;
        vm.ip = chunk.code.items.ptr;
        return vm.run();
    }

    fn run(vm: *VM) InterpreterResult {
        while (true) {
            if (build_mode == .Debug) {
                std.debug.print("        ", .{});
                var slot = &vm.stack;
                while (slot - vm.stack_top < 0) {
                    std.debug.print("[ ", .{});
                    debug.printValue(*slot);
                    std.debug.print(" ]", .{});
                    slot += 1;
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
                    vm.push(constant);
                    // debug.printValue(value);
                    // std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_NEGATE) => vm.push(-vm.pop()),
                @intFromEnum(OpCode.OP_ADD) => vm.binOp(BinOp.PLUS),
                @intFromEnum(OpCode.OP_SUBSTRUCT) => vm.binOp(BinOp.MINUS),
                @intFromEnum(OpCode.OP_MULTIPLY) => vm.binOp(BinOp.MULTIPLY),
                @intFromEnum(OpCode.OP_DIVIDE) => vm.binOp(BinOp.DIVIDE),
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

    inline fn binOp(vm: *VM, comptime operator: BinOp) void {
        const op_right = vm.pop();
        const op_left = vm.pop();
        switch (operator) {
            BinOp.PLUS => vm.push(op_left + op_right),
            BinOp.MINUS => vm.push(op_left - op_right),
            BinOp.MULTIPLY => vm.push(op_left * op_right),
            BinOp.DIVIDE => vm.push(op_left / op_right),
        }
    }
};
