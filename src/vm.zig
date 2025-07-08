const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const value_mod = @import("value.zig");
const object = @import("object.zig");
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;
const Value = value_mod.Value;
const Object = object.Object;
const build_mode = @import("builtin").mode;

const STACK_MAX_SIZE = 256;

pub const InterpreterError = error{
    BinOpError,
};

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
    GREATER,
    LESS,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: ArrayList(Value),
    arena_alloc: Allocator,
    obj_alloc: Allocator,
    gpa: std.heap.GeneralPurposeAllocator(.{}),

    pub fn init(allocator: Allocator) VM {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var vm: VM = .{ .chunk = undefined, .ip = undefined, .stack = ArrayList(Value).init(allocator), .arena_alloc = allocator, .gpa = gpa, .obj_alloc = gpa.allocator() };
        vm.resetStack();
        return vm;
    }
    pub fn deinit(self: *VM) void {
        self.stack.deinit();
        const leaked = self.gpa.deinit();
        if (leaked == .leak) {
            std.log.err("Memory leak detected in object allocator", .{});
        }
    }
    fn peek(self: *VM, offset: usize) Value {
        return self.stack.items[self.stack.items.len - 1 - offset];
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

    pub fn interpret(vm: *VM, source: []const u8) !InterpreterResult {
        var chunk = Chunk.init(vm.arena_alloc);
        try compiler.compile(source, &chunk, vm.arena_alloc, vm.gpa.allocator()); // catch return InterpreterResult.INTERPRET_COMPILE_ERROR;
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
                @intFromEnum(OpCode.OP_NIL) => {
                    try vm.push(Value.initNil());
                },
                @intFromEnum(OpCode.OP_TRUE) => {
                    try vm.push(Value.initBoolean(true));
                },
                @intFromEnum(OpCode.OP_FALSE) => {
                    try vm.push(Value.initBoolean(false));
                },
                @intFromEnum(OpCode.OP_NEGATE) => {
                    if (!vm.peek(0).isNumber()) {
                        try vm.throw("Operand must be a number");
                    }
                    try vm.push(Value.initNumber(-vm.pop().asNumber()));
                },
                @intFromEnum(OpCode.OP_ADD) => {
                    if (vm.peek(0).isObject() and vm.peek(0).asObject().isObjString() and vm.peek(1).isObject() and vm.peek(1).asObject().isObjString()) {
                        try vm.concat();
                    } else if (vm.peek(0).isNumber() and vm.peek(1).isNumber()) {
                        try vm.binOp(BinOp.PLUS);
                    } else {
                        try vm.throw("Operands must be a two number or two strings");
                    }
                },
                @intFromEnum(OpCode.OP_SUBSTRUCT) => try vm.binOp(BinOp.MINUS),
                @intFromEnum(OpCode.OP_MULTIPLY) => try vm.binOp(BinOp.MULTIPLY),
                @intFromEnum(OpCode.OP_DIVIDE) => try vm.binOp(BinOp.DIVIDE),
                @intFromEnum(OpCode.OP_NOT) => try vm.push(Value.initBoolean(isFalsey(vm.pop()))),
                @intFromEnum(OpCode.OP_EQUAL) => {
                    const v2 = vm.pop();
                    const v1 = vm.pop();
                    const res = Value.isEqual(v1, v2);
                    try vm.push(Value.initBoolean(res));
                },
                @intFromEnum(OpCode.OP_GREATER) => try vm.binOp(BinOp.GREATER),
                @intFromEnum(OpCode.OP_LESS) => try vm.binOp(BinOp.LESS),
                else => {
                    std.debug.print("Unkown instruction {d}\n", .{instruction});
                    @panic("Error");
                },
            }
        }
    }

    fn throw(self: *VM, msg: []const u8) !void {
        const instruction_index = self.ip - self.chunk.code.items.ptr - 1; // not sure this works
        std.debug.print("Error [line {d}]: {s}\n", .{ self.chunk.lines.items[instruction_index], msg });
    }

    inline fn readByte(vm: *VM) u8 {
        const ret = vm.ip[0];
        vm.ip += 1;
        return ret;
    }

    inline fn readConstant(vm: *VM) Value {
        return vm.chunk.constants.values.items[readByte(vm)];
    }

    fn binOp(vm: *VM, comptime operator: BinOp) !void {
        if (!vm.peek(0).isNumber() or !vm.peek(1).isNumber()) {
            try vm.throw("Operands must be a number");
            return InterpreterError.BinOpError;
        }
        const op_right = vm.pop();
        const op_left = vm.pop();
        switch (operator) {
            BinOp.PLUS => try vm.push(Value.initNumber(op_left.asNumber() + op_right.asNumber())),
            BinOp.MINUS => try vm.push(Value.initNumber(op_left.asNumber() - op_right.asNumber())),
            BinOp.MULTIPLY => try vm.push(Value.initNumber(op_left.asNumber() * op_right.asNumber())),
            BinOp.DIVIDE => try vm.push(Value.initNumber(op_left.asNumber() / op_right.asNumber())),
            BinOp.GREATER => try vm.push(Value.initBoolean(op_left.asNumber() > op_right.asNumber())),
            BinOp.LESS => try vm.push(Value.initBoolean(op_left.asNumber() < op_right.asNumber())),
        }
    }
    fn concat(vm: *VM) !void {
        const str2 = vm.pop().asObject().asObjString();
        const str1 = vm.pop().asObject().asObjString();
        var alloc_str = try vm.gpa.allocator().alloc(u8, str1.str.len + str2.str.len);
        defer vm.obj_alloc.free(alloc_str);
        @memcpy(alloc_str[0..str1.str.len], str1.str);
        @memcpy(alloc_str[str1.str.len..], str2.str);
        try vm.push(Value.initObject(try Object.initObjString(
            alloc_str[0..],
            vm.gpa.allocator(),
        )));
    }
    fn isFalsey(val: Value) bool {
        return val.isNil() or (val.isBoolean() and !val.asBoolean());
    }
};
