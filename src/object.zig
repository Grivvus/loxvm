const std = @import("std");
const Chunk = @import("bytecode.zig").Chunk;
const Value = @import("value.zig").Value;

pub const ObjType = enum {
    OBJ_STRING,
    OBJ_FUNCTION,
    OBJ_CLOSURE,
    OBJ_NATIVE,
    OBJ_UPVALUE,
};

pub const NativeFn = *const (fn (argc: usize, args: []Value) Value);

pub const Object = struct {
    type_: ObjType,
    is_marked: bool = false,
    mem: *anyopaque,
    pub fn deinit(self: *Object, allocator: std.mem.Allocator) void {
        switch (self.type_) {
            .OBJ_STRING => self.asObjString().deinit(allocator),
            .OBJ_FUNCTION => self.asObjFunction().deinit(allocator),
            .OBJ_CLOSURE => self.asObjClosure().deinit(allocator),
            .OBJ_NATIVE => self.asObjNative().deinit(allocator),
            .OBJ_UPVALUE => self.asObjUpvalue().deinit(allocator),
        }
        allocator.destroy(self);
    }

    pub fn initObjString(allocator: std.mem.Allocator, src: []const u8) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{
            .type_ = .OBJ_STRING,
            .mem = try ObjString.init(allocator, src),
        };
        return obj;
    }

    pub fn initObjFunction(allocator: std.mem.Allocator, name: ?*ObjString) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{
            .type_ = .OBJ_FUNCTION,
            .mem = try ObjFunction.init(allocator, name),
        };
        return obj;
    }
    pub fn fromFunction(allocator: std.mem.Allocator, function: *ObjFunction) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{
            .type_ = .OBJ_FUNCTION,
            .mem = function,
        };
        return obj;
    }

    pub fn initObjClosure(allocator: std.mem.Allocator, function: *ObjFunction) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{
            .type_ = .OBJ_CLOSURE,
            .mem = try ObjClosure.init(allocator, function),
        };
        return obj;
    }
    pub fn fromClosure(allocator: std.mem.Allocator, closure: *ObjClosure) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{
            .type_ = .OBJ_CLOSURE,
            .mem = closure,
        };
        return obj;
    }

    pub fn initObjNative(
        allocator: std.mem.Allocator,
        function: NativeFn,
    ) !*Object {
        const obj = try allocator.create(Object);
        obj.* = .{
            .type_ = .OBJ_NATIVE,
            .mem = try ObjNative.init(allocator, function),
        };
        return obj;
    }

    pub fn isObjString(self: Object) bool {
        return self.type_ == .OBJ_STRING;
    }
    pub fn isObjFunction(self: Object) bool {
        return self.type_ == .OBJ_FUNCTION;
    }
    pub fn isObjClosure(self: Object) bool {
        return self.type_ == .OBJ_CLOSURE;
    }
    pub fn isObjNative(self: Object) bool {
        return self.type_ == .OBJ_NATIVE;
    }
    pub fn isObjUpvalue(self: Object) bool {
        return self.type_ == .OBJ_UPVALUE;
    }

    pub fn asObjString(self: Object) *ObjString {
        if (self.type_ != .OBJ_STRING) {
            @panic("This object should be a string");
        }
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjFunction(self: Object) *ObjFunction {
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjClosure(self: Object) *ObjClosure {
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjNative(self: Object) *ObjNative {
        return @ptrCast(@alignCast(self.mem));
    }
    pub fn asObjUpvalue(self: Object) *ObjUpvalue {
        return @ptrCast(@alignCast(self.mem));
    }

    pub fn printObject(self: Object) void {
        switch (self.type_) {
            .OBJ_STRING => std.debug.print("{s}", .{self.asObjString().str}),
            .OBJ_FUNCTION => self.asObjFunction().print(),
            .OBJ_CLOSURE => self.asObjClosure().function.print(),
            .OBJ_NATIVE => std.debug.print("<native fn>", .{}),
            .OBJ_UPVALUE => std.debug.print("upvalue", .{}),
        }
    }
};

pub const ObjString = struct {
    str: []u8,
    pub fn init(allocator: std.mem.Allocator, source: []const u8) !*ObjString {
        const obj = try allocator.create(ObjString);
        const alloc_str = try allocator.alloc(u8, source.len);
        std.mem.copyBackwards(u8, alloc_str, source);
        obj.*.str = alloc_str;
        return obj;
    }
    pub fn deinit(self: *ObjString, allocator: std.mem.Allocator) void {
        allocator.free(self.str);
        allocator.destroy(self);
    }
};

pub const ObjFunction = struct {
    arity: u32,
    upvalue_cnt: usize,
    chunk: Chunk,
    name: ?*ObjString,
    pub fn init(allocator: std.mem.Allocator, name: ?*ObjString) !*ObjFunction {
        var function = try allocator.create(ObjFunction);
        const chunk = Chunk.init(allocator);
        function.chunk = chunk;
        function.arity = 0;
        function.upvalue_cnt = 0;
        function.name = name;
        return function;
    }
    pub fn deinit(self: *ObjFunction, allocator: std.mem.Allocator) void {
        self.chunk.deinit(allocator);
        if (self.name != null) {
            self.name.deinit(allocator);
        }
        allocator.destroy(self);
    }

    pub fn print(self: *ObjFunction) void {
        if (self.name != null) {
            std.debug.print("<fn {s}>", .{self.name.?.str});
        } else {
            std.debug.print("<script>", .{});
        }
    }
};

pub const ObjNative = struct {
    function: NativeFn,
    fn init(allocator: std.mem.Allocator, function: NativeFn) !*ObjNative {
        const obj_native = try allocator.create(ObjNative);
        obj_native.*.function = function;
        return obj_native;
    }

    fn deinit(self: *ObjNative, allocator: std.mem.Allocator) void {
        allocator.destroy(self);
    }
};

pub const ObjClosure = struct {
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,
    upvalue_cnt: usize,
    pub fn init(allocator: std.mem.Allocator, function: *ObjFunction) !*ObjClosure {
        const closure = try allocator.create(ObjClosure);
        const upvalues = try allocator.alloc(*ObjUpvalue, function.upvalue_cnt);
        closure.* = .{
            .function = function,
            .upvalues = upvalues,
            .upvalue_cnt = function.upvalue_cnt,
        };
        return closure;
    }
    pub fn deinit(self: *ObjClosure, allocator: std.mem.Allocator) void {
        allocator.free(self.upvalues);
        allocator.destroy(self);
    }
};

pub const ObjUpvalue = struct {
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
    pub fn init(allocator: std.mem.Allocator, location: *Value) !*ObjUpvalue {
        const upvalue = try allocator.create(ObjUpvalue);
        upvalue.* = .{
            .location = location,
            .next = null,
            .closed = undefined,
        };
        return upvalue;
    }
    pub fn deinit(self: *ObjUpvalue, allocator: std.mem.Allocator) void {
        if (self.next != null) {
            self.next.?.deinit(allocator);
        }
        allocator.destroy(self);
    }
};
