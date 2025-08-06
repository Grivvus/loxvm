const std = @import("std");
const ArrayList = std.ArrayList;
const HashMap = std.StringHashMap;
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
const ObjType = object.ObjType;
const ObjFunction = object.ObjFunction;
const build_mode = @import("builtin").mode;

const FRAMES_MAX = 64;
const STACK_MAX_SIZE = FRAMES_MAX * std.math.maxInt(u8);

fn clockNative(argc: usize, args: []Value) Value {
    _ = argc;
    _ = args;
    return Value.initNumber(@floatFromInt(std.time.timestamp()));
}

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

pub const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: []Value,
    slots_start_index: usize,
};

pub const VM = struct {
    frames: []CallFrame,
    frame_count: u32,
    stack: [STACK_MAX_SIZE]Value,
    stack_top_index: usize,
    globals: HashMap(Value),

    arena_alloc: Allocator,
    obj_alloc: Allocator,
    gpa: std.heap.GeneralPurposeAllocator(.{}),

    pub fn init(allocator: Allocator) !VM {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        var vm = VM{
            .frames = try allocator.alloc(CallFrame, FRAMES_MAX),
            .frame_count = 0,
            .stack = undefined,
            .stack_top_index = 0,
            .globals = HashMap(Value).init(allocator),
            .arena_alloc = allocator,
            .gpa = gpa,
            .obj_alloc = gpa.allocator(),
        };
        vm.resetStack();
        try vm.defineNative("clock", clockNative);
        return vm;
    }
    pub fn deinit(self: *VM) void {
        self.arena_alloc.free(self.frames);
        self.globals.deinit();
        const leaked = self.gpa.deinit();
        if (leaked == .leak) {
            std.log.err("Memory leak detected in object allocator", .{});
        }
    }
    fn peek(self: *VM, offset: usize) Value {
        return self.stack[self.stack_top_index - 1 - offset];
    }

    fn callValue(self: *VM, callee: Value, arg_count: u8) !bool {
        if (callee.isObject()) {
            const obj = callee.asObject();
            switch (obj.type_) {
                ObjType.OBJ_FUNCTION => {
                    return try self.call(obj.asObjFunction(), arg_count);
                },
                ObjType.OBJ_NATIVE => {
                    const native_fn = obj.asObjNative().function;
                    const result = native_fn(
                        arg_count,
                        self.stack[self.stack_top_index - arg_count .. self.stack_top_index],
                    );
                    self.stack_top_index -= arg_count + 1;
                    self.push(result);
                    return true;
                },
                else => {
                    _ = self.throw("Not callable object");
                    return false;
                },
            }
        }
        return false;
    }

    fn call(self: *VM, function: *ObjFunction, arg_count: u8) !bool {
        if (arg_count != function.arity) {
            _ = self.throw(try std.fmt.allocPrint(self.arena_alloc, "Expected {d} arguments, but got {d}", .{ function.arity, arg_count }));
            return false;
        }

        if (self.frame_count >= FRAMES_MAX) {
            _ = self.throw("Stack overflow");
            return false;
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.* = .{
            .function = function,
            .ip = function.chunk.code.items.ptr,
            .slots = self.stack[0..],
            .slots_start_index = self.stack_top_index - arg_count - 1,
        };
        return true;
    }

    fn resetStack(self: *VM) void {
        self.stack_top_index = 0;
    }
    fn push(self: *VM, val: Value) void {
        if (self.stack_top_index >= STACK_MAX_SIZE) {
            @panic("Stack overflow");
        }
        self.stack[self.stack_top_index] = val;
        self.stack_top_index += 1;
    }
    fn pop(self: *VM) Value {
        if (self.stack_top_index == 0) {
            @panic("Poped from empty stack");
        }
        self.stack_top_index -= 1;
        const val = self.stack[self.stack_top_index];
        return val;
    }

    pub fn interpret(vm: *VM, source: []const u8) !InterpreterResult {
        const function = try compiler.compile(source, vm.arena_alloc, vm.gpa.allocator()); // catch return InterpreterResult.INTERPRET_COMPILE_ERROR;
        vm.push(Value.initObject(try Object.initObjFunction(vm.gpa.allocator(), null)));
        _ = try vm.call(function, 0);

        return vm.run() catch InterpreterResult.INTERPRET_RUNTIME_ERROR;
    }

    fn run(vm: *VM) !InterpreterResult {
        var frame: *CallFrame = &vm.frames[vm.frame_count - 1];
        while (true) {
            if (build_mode == .Debug) {
                std.debug.print("        ", .{});
                for (0..vm.stack_top_index) |index| {
                    std.debug.print("[ ", .{});
                    debug.printValue(vm.stack[index]);
                    std.debug.print(" ]", .{});
                }
                std.debug.print("\n", .{});
                _ = debug.disassembleInstruction(frame.function.chunk, frame.ip - frame.function.chunk.code.items.ptr);
            }

            const instruction = readByte(vm);
            switch (instruction) {
                @intFromEnum(OpCode.OP_JUMP) => {
                    const offset = readShort(vm);
                    frame.ip += offset;
                },
                @intFromEnum(OpCode.OP_JUMP_IF_FALSE) => {
                    const offset = readShort(vm);
                    if (isFalsey(vm.peek(0))) {
                        frame.ip += offset;
                    }
                },
                @intFromEnum(OpCode.OP_LOOP) => {
                    const offset = readShort(vm);
                    frame.ip -= offset;
                },
                @intFromEnum(OpCode.OP_RETURN) => {
                    const result = pop(vm);
                    vm.frame_count -= 1;
                    if (vm.frame_count == 0) {
                        _ = pop(vm);
                        return InterpreterResult.INTERPRET_OK;
                    }
                    vm.stack_top_index = frame.slots_start_index;
                    vm.push(result);
                    frame = &vm.frames[vm.frame_count - 1];
                },
                @intFromEnum(OpCode.OP_CONSTANT) => {
                    const constant = readConstant(vm);
                    vm.push(constant);
                    // debug.printValue(value);
                    // std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_NIL) => {
                    vm.push(Value.initNil());
                },
                @intFromEnum(OpCode.OP_TRUE) => {
                    vm.push(Value.initBoolean(true));
                },
                @intFromEnum(OpCode.OP_FALSE) => {
                    vm.push(Value.initBoolean(false));
                },
                @intFromEnum(OpCode.OP_NEGATE) => {
                    if (!vm.peek(0).isNumber()) {
                        return vm.throw("Operand must be a number");
                    }
                    vm.push(Value.initNumber(-vm.pop().asNumber()));
                },
                @intFromEnum(OpCode.OP_ADD) => {
                    if (vm.peek(0).isObject() and vm.peek(0).asObject().isObjString() and vm.peek(1).isObject() and vm.peek(1).asObject().isObjString()) {
                        try vm.concat();
                    } else if (vm.peek(0).isNumber() and vm.peek(1).isNumber()) {
                        const res = try vm.binOp(BinOp.PLUS);
                        switch (res) {
                            .INTERPRET_RUNTIME_ERROR => return InterpreterResult.INTERPRET_RUNTIME_ERROR,
                            else => {},
                        }
                    } else {
                        return vm.throw("Operands must be a two number or two strings");
                    }
                },
                @intFromEnum(OpCode.OP_SUBSTRUCT) => {
                    const res = try vm.binOp(BinOp.MINUS);
                    switch (res) {
                        .INTERPRET_RUNTIME_ERROR => return InterpreterResult.INTERPRET_RUNTIME_ERROR,
                        else => {},
                    }
                },
                @intFromEnum(OpCode.OP_MULTIPLY) => {
                    const res = try vm.binOp(BinOp.MULTIPLY);
                    switch (res) {
                        .INTERPRET_RUNTIME_ERROR => return InterpreterResult.INTERPRET_RUNTIME_ERROR,
                        else => {},
                    }
                },
                @intFromEnum(OpCode.OP_DIVIDE) => {
                    const res = try vm.binOp(BinOp.DIVIDE);
                    switch (res) {
                        .INTERPRET_RUNTIME_ERROR => return InterpreterResult.INTERPRET_RUNTIME_ERROR,
                        else => {},
                    }
                },
                @intFromEnum(OpCode.OP_NOT) => vm.push(Value.initBoolean(isFalsey(vm.pop()))),
                @intFromEnum(OpCode.OP_EQUAL) => {
                    const v2 = vm.pop();
                    const v1 = vm.pop();
                    const res = Value.isEqual(v1, v2);
                    vm.push(Value.initBoolean(res));
                },
                @intFromEnum(OpCode.OP_GREATER) => {
                    const res = try vm.binOp(BinOp.GREATER);
                    switch (res) {
                        .INTERPRET_RUNTIME_ERROR => return InterpreterResult.INTERPRET_RUNTIME_ERROR,
                        else => {},
                    }
                },
                @intFromEnum(OpCode.OP_LESS) => {
                    const res = try vm.binOp(BinOp.LESS);
                    switch (res) {
                        .INTERPRET_RUNTIME_ERROR => return InterpreterResult.INTERPRET_RUNTIME_ERROR,
                        else => {},
                    }
                },
                @intFromEnum(OpCode.OP_PRINT) => {
                    vm.pop().printValue();
                    std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.OP_POP) => {
                    _ = vm.pop();
                },
                @intFromEnum(OpCode.OP_DEFINE_GLOBAL) => {
                    const name = readConstant(vm).asObject().asObjString();
                    try vm.globals.put(name.str, peek(vm, 0));
                    _ = pop(vm);
                },
                @intFromEnum(OpCode.OP_GET_GLOBAL) => {
                    const name = readConstant(vm).asObject().asObjString();
                    const value = vm.globals.get(name.str);
                    if (value == null) {
                        return throw(vm, try std.fmt.allocPrint(vm.arena_alloc, "Undefined variable '{s}'", .{name.str}));
                    }
                    vm.push(value.?);
                },
                @intFromEnum(OpCode.OP_SET_GLOBAL) => {
                    const name = readConstant(vm).asObject().asObjString();
                    if (!vm.globals.contains(name.str)) {
                        return throw(vm, try std.fmt.allocPrint(vm.arena_alloc, "Undefined variable '{s}'", .{name.str}));
                    }
                    try vm.globals.put(name.str, vm.peek(0));
                },
                @intFromEnum(OpCode.OP_GET_LOCAL) => {
                    const slot = readByte(vm);
                    vm.push(frame.slots[slot + frame.slots_start_index]);
                },
                @intFromEnum(OpCode.OP_SET_LOCAL) => {
                    const slot = readByte(vm);
                    frame.slots[slot + frame.slots_start_index] = vm.peek(0);
                },
                @intFromEnum(OpCode.OP_CALL) => {
                    const arg_count = readByte(vm);
                    if (!(try callValue(vm, vm.peek(arg_count), arg_count))) {
                        return vm.throw("Error while calling");
                    }
                    const current_frame = vm.frames[vm.frame_count - 1];
                    _ = current_frame;
                },
                else => {
                    std.debug.print("Unkown instruction {d}\n", .{instruction});
                    @panic("Error");
                },
            }
        }
        return InterpreterResult.INTERPRET_OK;
    }

    fn throw(self: *VM, msg: []const u8) InterpreterResult {
        std.debug.print("Error\n", .{});
        var i = self.frame_count;
        while (i > 0) {
            i -= 1;
            const frame = &self.frames[i];
            const function = frame.function;
            const instruction_index = frame.ip - function.chunk.code.items.ptr - 1;
            const line = function.chunk.lines.items[instruction_index];
            const function_name = if (function.name == null) "script" else function.name.?.str;
            std.debug.print("[line {d}] in {s}\n", .{ line, function_name });
        }
        std.debug.print("\n [[{s}]] \n", .{msg});
        return InterpreterResult.INTERPRET_RUNTIME_ERROR;
    }

    fn defineNative(
        self: *VM,
        name: []const u8,
        function: object.NativeFn,
    ) !void {
        self.push(Value.initObject(try Object.initObjString(
            name,
            self.obj_alloc,
        )));
        self.push(Value.initObject(try Object.initObjNative(
            self.obj_alloc,
            function,
        )));
        try self.globals.put(
            self.stack[0].asObject().asObjString().str,
            self.stack[1],
        );
        _ = self.pop();
        _ = self.pop();
    }

    inline fn readByte(vm: *VM) u8 {
        const curr_frame = &vm.frames[vm.frame_count - 1];
        const ret = curr_frame.ip[0];
        curr_frame.*.ip += 1;
        return ret;
    }

    inline fn readShort(vm: *VM) u16 {
        const curr_frame = &vm.frames[vm.frame_count - 1];
        const current_ip: u16 = @intCast(curr_frame.ip[0]);
        const offset = (current_ip << 8) | curr_frame.ip[1];
        curr_frame.ip += 2;
        return offset;
    }

    inline fn readConstant(vm: *VM) Value {
        return vm.frames[vm.frame_count - 1].function.chunk
            .constants.values.items[readByte(vm)];
    }

    fn binOp(vm: *VM, comptime operator: BinOp) !InterpreterResult {
        if (!vm.peek(0).isNumber() or !vm.peek(1).isNumber()) {
            return vm.throw("Operands must be a number");
        }
        const op_right = vm.pop();
        const op_left = vm.pop();
        switch (operator) {
            BinOp.PLUS => vm.push(Value.initNumber(op_left.asNumber() + op_right.asNumber())),
            BinOp.MINUS => vm.push(Value.initNumber(op_left.asNumber() - op_right.asNumber())),
            BinOp.MULTIPLY => vm.push(Value.initNumber(op_left.asNumber() * op_right.asNumber())),
            BinOp.DIVIDE => vm.push(Value.initNumber(op_left.asNumber() / op_right.asNumber())),
            BinOp.GREATER => vm.push(Value.initBoolean(op_left.asNumber() > op_right.asNumber())),
            BinOp.LESS => vm.push(Value.initBoolean(op_left.asNumber() < op_right.asNumber())),
        }
        return InterpreterResult.INTERPRET_OK;
    }
    fn concat(vm: *VM) !void {
        const str2 = vm.pop().asObject().asObjString();
        const str1 = vm.pop().asObject().asObjString();
        var alloc_str = try vm.gpa.allocator()
            .alloc(u8, str1.str.len + str2.str.len);
        defer vm.obj_alloc.free(alloc_str);
        @memcpy(alloc_str[0..str1.str.len], str1.str);
        @memcpy(alloc_str[str1.str.len..], str2.str);
        vm.push(Value.initObject(try Object.initObjString(
            alloc_str[0..],
            vm.gpa.allocator(),
        )));
    }
    fn isFalsey(val: Value) bool {
        return val.isNil() or (val.isBoolean() and !val.asBoolean());
    }
};
