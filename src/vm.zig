const std = @import("std");
const ArrayList = std.ArrayList;
const HashMap = std.HashMap;
const Allocator = std.mem.Allocator;
const bytecode = @import("bytecode.zig");
const value_mod = @import("value.zig");
const object = @import("object.zig");
const debug = @import("debug.zig");
const compiler = @import("compiler.zig");
const gc = @import("gc.zig");
const table = @import("table.zig");
const collectGarbage = gc.collectGarbage;
const Chunk = bytecode.Chunk;
const OpCode = bytecode.OpCode;
const Value = value_mod.Value;
const Object = object.Object;
const ObjType = object.ObjType;
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;
const ObjNative = object.ObjNative;
const ObjClosure = object.ObjClosure;
const ObjUpvalue = object.ObjUpvalue;
const ObjClass = object.ObjClass;
const ObjInstance = object.ObjInstance;
const ObjBoundMethod = object.ObjBoundMethod;
const Table = table.Table;
const build_mode = @import("builtin").mode;

const FRAMES_MAX = 64;
const STACK_MAX_SIZE = FRAMES_MAX * std.math.maxInt(u8);
const DEBUG_STRESS_GC = gc.DEBUG_STRESS_GC;

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
    closure: *ObjClosure,
    ip: [*]u8,
    slots: []Value,
    slots_start_index: usize,
};

pub const VM = struct {
    frames: []CallFrame,
    frame_count: u32 = 0,
    stack: [STACK_MAX_SIZE]Value,
    stack_top_index: usize = 0,
    open_upvalues: ?*ObjUpvalue = null,
    globals: Table,
    objects: ?*Object = null,

    bytes_allocated: usize = 0,
    next_gc: usize = 1024 * 1024,
    gray_stack: ArrayList(*Object),

    init_str: *ObjString = undefined,

    arena_alloc: Allocator,
    obj_alloc: Allocator,
    gpa: std.heap.GeneralPurposeAllocator(.{}),

    pub fn init(allocator: Allocator) !VM {
        var gpa: std.heap.GeneralPurposeAllocator(.{}) = .init;
        var vm = VM{
            .frames = try allocator.alloc(CallFrame, FRAMES_MAX),
            .stack = undefined,
            .globals = Table.init(allocator),
            .gray_stack = try ArrayList(*Object).initCapacity(allocator, 1024),
            .arena_alloc = allocator,
            .gpa = gpa,
            .obj_alloc = gpa.allocator(),
        };
        vm.resetStack();
        vm.init_str = ObjString.init(&vm, vm.obj_alloc, "init");
        try vm.defineNative("clock", clockNative);
        return vm;
    }
    pub fn deinit(self: *VM) void {
        self.stack_top_index = 0;
        self.frame_count = 0;
        self.open_upvalues = null;

        while (self.objects) |obj| {
            self.objects = obj.next;
            self.bytes_allocated -= obj.deinit();
        }

        self.globals.clearRetainingCapacity();
        self.globals.deinit();
        self.gray_stack.deinit(self.arena_alloc);
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
                ObjType.OBJ_CLOSURE => {
                    return try self.call(obj.as(ObjClosure), arg_count);
                },
                ObjType.OBJ_NATIVE => {
                    const native_fn = obj.as(ObjNative).function;
                    const result = native_fn(
                        arg_count,
                        self.stack[self.stack_top_index - arg_count .. self.stack_top_index],
                    );
                    self.stack_top_index -= arg_count + 1;
                    self.push(result);
                    return true;
                },
                ObjType.OBJ_CLASS => {
                    const class = callee.asObject().as(ObjClass);
                    self.stack[self.stack_top_index - arg_count - 1] = Value.initObject(&ObjInstance.init(
                        self,
                        self.obj_alloc,
                        class,
                    ).object);
                    if (class.methods.get(self.init_str)) |init_method| {
                        return call(self, init_method.asObject().as(ObjClosure), arg_count);
                    } else if (arg_count != 0) {
                        _ = self.throw(try std.fmt.allocPrint(self.arena_alloc, "expected 0 arguments but got {d}", .{arg_count}));
                        return false;
                    }
                    return true;
                },
                ObjType.OBJ_BOUND_METHOD => {
                    const bound_method = callee.asObject().as(ObjBoundMethod);
                    self.stack[self.stack_top_index - 1 - arg_count] = bound_method.reciever;
                    return self.call(bound_method.method, arg_count);
                },
                else => {
                    _ = self.throw("Not callable object");
                    return false;
                },
            }
        }
        return false;
    }

    fn captureUpvalue(self: *VM, local: *Value) !*ObjUpvalue {
        var prev_upvalue: ?*ObjUpvalue = null;
        var upvalue = self.open_upvalues;
        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }
        if (upvalue != null and local == upvalue.?.location) {
            return upvalue.?;
        }

        const created_upvalue = ObjUpvalue.init(self, self.obj_alloc, local);

        if (prev_upvalue == null) {
            self.open_upvalues = created_upvalue;
        } else {
            prev_upvalue.?.next = created_upvalue;
        }
        return created_upvalue;
    }

    fn closeUpvalue(self: *VM, last: *Value) void {
        while (self.open_upvalues != null and @intFromPtr(self.open_upvalues.?.location) >= @intFromPtr(last)) {
            var upvalue = self.open_upvalues;
            upvalue.?.closed = upvalue.?.location.*;
            upvalue.?.location = &upvalue.?.closed;
            self.open_upvalues = upvalue.?.next;
        }
    }

    fn call(self: *VM, closure: *ObjClosure, arg_count: u8) !bool {
        if (arg_count != closure.function.arity) {
            _ = self.throw(try std.fmt.allocPrint(
                self.arena_alloc,
                "Expected {d} arguments, but got {d}",
                .{ closure.function.arity, arg_count },
            ));
            return false;
        }

        if (self.frame_count >= FRAMES_MAX) {
            _ = self.throw("Stack overflow");
            return false;
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.* = .{
            .closure = closure,
            .ip = closure.function.chunk.code.items.ptr,
            .slots = &self.stack,
            .slots_start_index = self.stack_top_index - arg_count - 1,
        };
        return true;
    }

    fn resetStack(self: *VM) void {
        self.stack_top_index = 0;
        self.open_upvalues = null;
    }
    pub fn push(self: *VM, val: Value) void {
        if (self.stack_top_index >= STACK_MAX_SIZE) {
            @panic("Stack overflow");
        }
        self.stack[self.stack_top_index] = val;
        self.stack_top_index += 1;
    }
    pub fn pop(self: *VM) Value {
        if (self.stack_top_index == 0) {
            @panic("Poped from empty stack");
        }
        self.stack_top_index -= 1;
        const val = self.stack[self.stack_top_index];
        return val;
    }

    pub fn interpret(vm: *VM, source: []const u8) !InterpreterResult {
        const function = try compiler.compile(
            source,
            vm,
            vm.arena_alloc,
            vm.gpa.allocator(),
        ); // catch return InterpreterResult.INTERPRET_COMPILE_ERROR;
        vm.push(Value.initObject(&function.object));
        const closure = ObjClosure.init(vm, vm.obj_alloc, function);
        _ = vm.pop();
        vm.push(Value.initObject(&closure.object));
        _ = try vm.call(closure, 0);

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
                _ = debug.disassembleInstruction(frame.closure.function.chunk, frame.ip - frame.closure.function.chunk.code.items.ptr);
            }

            if (DEBUG_STRESS_GC) {
                collectGarbage(vm);
            }
            if (vm.bytes_allocated >= vm.next_gc) {
                collectGarbage(vm);
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
                    vm.closeUpvalue(&frame.slots[frame.slots_start_index]);
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
                    const name = readConstant(vm).asObject().as(ObjString);
                    try vm.globals.put(name, peek(vm, 0));
                    _ = pop(vm);
                },
                @intFromEnum(OpCode.OP_GET_GLOBAL) => {
                    const name = readConstant(vm).asObject().as(ObjString);
                    const value = vm.globals.get(name);
                    if (value == null) {
                        return throw(vm, try std.fmt.allocPrint(vm.arena_alloc, "Undefined variable '{s}'", .{name.str}));
                    }
                    vm.push(value.?);
                },
                @intFromEnum(OpCode.OP_SET_GLOBAL) => {
                    const name = readConstant(vm).asObject().as(ObjString);
                    if (!vm.globals.contains(name)) {
                        return throw(vm, try std.fmt.allocPrint(vm.arena_alloc, "Undefined variable '{s}'", .{name.str}));
                    }
                    try vm.globals.put(name, vm.peek(0));
                },
                @intFromEnum(OpCode.OP_GET_LOCAL) => {
                    const slot = readByte(vm);
                    vm.push(frame.slots[slot + frame.slots_start_index]);
                },
                @intFromEnum(OpCode.OP_SET_LOCAL) => {
                    const slot = readByte(vm);
                    frame.slots[slot + frame.slots_start_index] = vm.peek(0);
                },
                @intFromEnum(OpCode.OP_GET_UPVALUE) => {
                    const slot = readByte(vm);
                    vm.push(frame.closure.upvalues[slot].location.*);
                },
                @intFromEnum(OpCode.OP_SET_UPVALUE) => {
                    const slot = readByte(vm);
                    frame.closure.upvalues[slot].location.* = vm.peek(0);
                },
                @intFromEnum(OpCode.OP_GET_PROPERTY) => {
                    if (!vm.peek(0).asObject().isObjInstance()) {
                        return vm.throw("Only instances have properties");
                    }
                    const instance = vm.peek(0).asObject().as(ObjInstance);
                    const name = readConstant(vm).asObject().as(ObjString);
                    if (instance.fields.get(name)) |value| {
                        _ = vm.pop();
                        vm.push(value);
                    } else {
                        if (!try vm.bindMethod(instance.class, name)) {
                            return InterpreterResult.INTERPRET_RUNTIME_ERROR;
                        }
                    }
                },
                @intFromEnum(OpCode.OP_SET_PROPERTY) => {
                    const instance = vm.peek(1).asObject().as(ObjInstance);
                    const value = vm.peek(0);
                    const name = readConstant(vm).asObject().as(ObjString);
                    try instance.fields.put(name, value);
                    const poped_value = vm.pop();
                    _ = vm.pop();
                    vm.push(poped_value);
                },
                @intFromEnum(OpCode.OP_CALL) => {
                    const arg_count = readByte(vm);
                    const callee = vm.peek(arg_count);
                    if (!(try callValue(vm, callee, arg_count))) {
                        return vm.throw("Error while calling");
                    }
                    frame = &vm.frames[vm.frame_count - 1];
                },
                @intFromEnum(OpCode.OP_CLOSURE) => {
                    const function = readConstant(vm).asObject().as(ObjFunction);
                    const closure = Value.initObject(&ObjClosure.init(vm, vm.obj_alloc, function).object);
                    const obj_closure = closure.asObject().as(ObjClosure);

                    for (0..function.upvalue_cnt) |i| {
                        const is_local = readByte(vm);
                        const index = readByte(vm);
                        if (is_local != 0) {
                            obj_closure.upvalues[i] = try vm.captureUpvalue(
                                &frame.slots[frame.slots_start_index + index],
                            );
                        } else {
                            obj_closure.upvalues[i] = frame.closure.upvalues[index];
                        }
                    }

                    vm.push(closure);
                },
                @intFromEnum(OpCode.OP_CLOSE_UPVALUE) => {
                    vm.closeUpvalue(&vm.stack[vm.stack_top_index - 1]);
                    _ = vm.pop();
                },
                @intFromEnum(OpCode.OP_CLASS) => {
                    const class = ObjClass.init(
                        vm,
                        vm.obj_alloc,
                        readConstant(vm).asObject().as(ObjString),
                    );
                    vm.push(Value.initObject(&class.object));
                },
                @intFromEnum(OpCode.OP_METHOD) => {
                    try defineMethod(vm, readConstant(vm).asObject().as(ObjString));
                },
                @intFromEnum(OpCode.OP_INVOKE) => {
                    const method = readConstant(vm).asObject().as(ObjString);
                    const arg_cnt = readByte(vm);
                    if (!try invoke(vm, method, arg_cnt)) {
                        return InterpreterResult.INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &vm.frames[vm.frame_count - 1];
                },
                @intFromEnum(OpCode.OP_SUPER_INVOKE) => {
                    const method = readConstant(vm).asObject().as(ObjString);
                    const arg_cnt = readByte(vm);
                    const superclass = vm.pop().asObject().as(ObjClass);
                    if (!try vm.invokeFromClass(superclass, method, arg_cnt)) {
                        return InterpreterResult.INTERPRET_RUNTIME_ERROR;
                    }
                    frame = &vm.frames[vm.frame_count - 1];
                },
                @intFromEnum(OpCode.OP_INHERIT) => {
                    if (!vm.peek(1).asObject().isObjClass()) {
                        return vm.throw("Can't inherit not from a class");
                    }
                    const superclass = vm.peek(1).asObject().as(ObjClass);
                    const subclass = vm.peek(0).asObject().as(ObjClass);
                    var super_keys_iter = superclass.methods.keyIterator();
                    while (super_keys_iter.next()) |key| {
                        try subclass.methods.put(key.*, superclass.methods.get(key.*).?);
                    }
                    _ = vm.pop();
                },
                @intFromEnum(OpCode.OP_GET_SUPER) => {
                    const name = readConstant(vm).asObject().as(ObjString);
                    const superclass = vm.pop().asObject().as(ObjClass);
                    if (!try bindMethod(vm, superclass, name)) {
                        return InterpreterResult.INTERPRET_RUNTIME_ERROR;
                    }
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
            const function = frame.closure.function;
            const instruction_index = frame.ip - function.chunk.code.items.ptr - 1;
            const line = function.chunk.lines.items[instruction_index];
            const function_name = if (function.name == null) "script" else function.name.?.str;
            std.debug.print("[line {d}] in {s}\n", .{ line, function_name });
        }
        std.debug.print("\n [[{s}]] \n", .{msg});
        return InterpreterResult.INTERPRET_RUNTIME_ERROR;
    }

    fn bindMethod(self: *VM, class: *ObjClass, name: *ObjString) !bool {
        if (class.methods.get(name)) |method| {
            const bound = ObjBoundMethod.init(
                self,
                self.obj_alloc,
                self.peek(0),
                method.asObject().as(ObjClosure),
            );
            _ = self.pop();
            self.push(Value.initObject(&bound.object));
            return true;
        } else {
            _ = self.throw(try std.fmt.allocPrint(self.arena_alloc, "Undefined property {s}", .{name.str}));
            return false;
        }
    }

    fn defineNative(
        self: *VM,
        name: []const u8,
        function: object.NativeFn,
    ) !void {
        self.push(Value.initObject(&ObjString.init(
            self,
            self.obj_alloc,
            name,
        ).object));
        self.push(Value.initObject(&ObjNative.init(
            self,
            self.obj_alloc,
            function,
        ).object));
        try self.globals.put(
            self.stack[0].asObject().as(ObjString),
            self.stack[1],
        );
        _ = self.pop();
        _ = self.pop();
    }

    fn defineMethod(
        self: *VM,
        name: *ObjString,
    ) !void {
        const method = self.peek(0);
        const class = self.peek(1).asObject().as(ObjClass);
        try class.methods.put(name, method);
        _ = self.pop();
    }

    fn invoke(self: *VM, name: *ObjString, arg_cnt: u8) !bool {
        const reciever = self.peek(arg_cnt);
        if (!reciever.asObject().isObjInstance()) {
            _ = self.throw("Only instances have methods");
            return false;
        }

        const instance = reciever.asObject().as(ObjInstance);
        if (instance.fields.get(name)) |value| {
            self.stack[self.stack_top_index - 1 - arg_cnt] = value;
            return self.callValue(value, arg_cnt);
        }
        return try self.invokeFromClass(instance.class, name, arg_cnt);
    }

    fn invokeFromClass(
        self: *VM,
        class: *ObjClass,
        name: *ObjString,
        arg_cnt: u8,
    ) !bool {
        if (class.methods.get(name)) |method| {
            return try self.call(method.asObject().as(ObjClosure), arg_cnt);
        } else {
            _ = self.throw(try std.fmt.allocPrint(self.arena_alloc, "Undefined property {s}", .{name.str}));
            return false;
        }
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
        return vm.frames[vm.frame_count - 1].closure.function.chunk
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
        const str2 = vm.peek(0).asObject().as(ObjString);
        const str1 = vm.peek(1).asObject().as(ObjString);
        var alloc_str = try vm.obj_alloc.alloc(u8, str1.str.len + str2.str.len);
        defer vm.obj_alloc.free(alloc_str);
        @memcpy(alloc_str[0..str1.str.len], str1.str);
        @memcpy(alloc_str[str1.str.len..], str2.str);
        _ = vm.pop();
        _ = vm.pop();
        vm.push(Value.initObject(&ObjString.init(
            vm,
            vm.obj_alloc,
            alloc_str[0..],
        ).object));
    }
    fn isFalsey(val: Value) bool {
        return val.isNil() or (val.isBoolean() and !val.asBoolean());
    }
};
